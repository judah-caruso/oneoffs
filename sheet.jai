/*
   Sheet is a tool to replace parts of source
   text with values defined in a definition
   sheet; sheet file for short, hence the name.

   For example, when writing a story, the names
   of characters/places/things may not be final
   and may need to be changed frequently. Sheet
   attempts to solve this problem by replacing
   special text within the source with values
   defined in the sheet file.

   The basic syntax of a sheet file:
      @foo -- This is a placeholder
         -- Below are fields.
         .foo! Default Name
         .field Value of field
         .another Field with a value
         .one-more_time 100

   Usage within the source text:
      Lorem @foo dolor sit amet,
      consectetur adipisicing elit, sed do
      eiusmod @foo.field.

   After calling 'sheet <input> <sheet>'
      Lorem Default Name dolor sit amet,
      consectetur adipisicing elit, sed do
      eiusmod Value of field.

   If a placeholder is found without a field
   (ie. '@foobar'), the default field '.name'
   will be used if present, otherwise the
   placeholder will be passed through as-is.

   To ovewrite the default field's name, create
   a field ending in '!':
      @foobar
         .default! The new default field.
         .name Will no longer be used by default

   On success, sheet will write the converted
   text to stdout.
*/

PROGRAM_NAME    :: "j-sheet";
PROGRAM_VERSION :: "1c (built on November 10, 2022)";
PROGRAM_USAGE   :: "@bold{.. usage} % @bold,red{<input file>} @bold,green{<sheet file>} [-allow-empty-fields]\n";

INPUT_HELP :: #string END
   @bold,red{input} is a file containing placeholder @ul{sequences}:
      The @@place is @@place.age years old!
END;

SHEET_HELP :: #string END
   @bold,green{sheet} is a file containing placeholder @ul{definitions}:
      @@place
         .name World
         .age  at least 5
END;

EMPTY_HELP :: #string END
   @bold{allow-empty-fields} allows fields to have @ul{empty} values:
      @@Foo
         .name
         .other Name is empty!
END;

#run ct_compile_to_binary(PROGRAM_NAME);

main :: () {
   cl_args := get_command_line_arguments();
   defer free(cl_args.data);

   input_filename: string;
   sheet_filename: string;

   allow_empty_fields := false;

   {
      args := cl_args;
      args.count -= 1;
      args.data  += 1;

      if !args.count {
         color_print(PROGRAM_USAGE, PROGRAM_NAME, stderr = true);
         return;
      }

      if args[0] == {
         case "-h"; #through;
         case "-help"; #through;
         case "--help";
            color_print(PROGRAM_USAGE, PROGRAM_NAME, stderr = true);
            color_print("\n%\n%\n%\n", INPUT_HELP, SHEET_HELP, EMPTY_HELP, stderr = true);
            return;

         case "-v"; #through;
         case "-version"; #through;
         case "--version";
            color_print("@bold{.. %} v%\n", PROGRAM_NAME, PROGRAM_VERSION, stderr = true);
            return;
      }

      input_filename = args[0];

      args.count -= 1;
      args.data  += 1;

      if !args.count {
         color_print("@red,bold{.. error} expected sheet file!\n", stderr = true);
         exit(1);
         return;
      }

      sheet_filename = args[0];

      if args.count > 1 {
         if args[1] == {
            case "-allow-empty-fields";
               allow_empty_fields = true;
            case;
               color_print("@red,bold{.. error} unknown flag @yellow{'%'}\n", args[1], stderr = true);
               exit(1);
               return;
         }
      }
   }

   source, ok := read_entire_file(sheet_filename);
   if !ok {
      color_print("@red,bold{.. error} unable to open file: @bold{%}\n", sheet_filename, stderr = true);
      exit(1);
      return;
   }

   placeholders: Table(string, *Placeholder_Map);
   current_placeholder: *Placeholder_Map;

   { // Parse the sheet file
      lines := split(source, "\n");
      for lines {
         line := trim(it);
         if !line.count continue;

         chr, ok, width := consume_rune(*line);
         if !ok continue;

         if chr == {
            case #char "@";  // Start of placeholder
               name := line;
               name.count = 0;

               while line.count {
                  chr, ok, width = peek_rune(line);
                  if !ok || !valid_char(chr) break;

                  name.count += width;
                  consume_rune(*line);
               }

               // invalid name was found
               if !name.count {
                  continue;
               }

               current_placeholder = New(Placeholder_Map);
               table_add(*placeholders, name, current_placeholder);
               continue;

            case #char "."; // start of field
               field := line;
               field.count = 0;

               while line.count {
                  chr, ok, width = peek_rune(line);
                  if !ok || !valid_char(chr) break;

                  field.count += width;
                  consume_rune(*line);
               }

               change_default_field: bool;

               chr, ok = peek_rune(line);
               if ok && chr == #char "!" {
                  consume_rune(*line);
                  change_default_field = true;
               }

               value := trim(line);

               // invalid field
               if !field.count || (!value.count && !allow_empty_fields) {
                  color_print("@red,bold{.. error} invalid line!\n   @bold{%}\n", it, stderr = true);
                  exit(1);
                  return;
               }

               if !current_placeholder {
                  color_print("@red,bold{.. error} field without placeholder!\n   @bold{%}\n", it, stderr = true);
                  exit(1);
                  return;
               }

               if change_default_field {
                  current_placeholder.default_field = field;
               }

               table_add(current_placeholder, field, value);
               continue;

            case #char "-"; // start of comment
               chr, ok = peek_rune(line);
               if ok && chr == #char "-" continue; // skip entire line

            case;
               color_print("@red,bold{.. error} invalid line!\n   @bold{%}\n", it, stderr = true);
               exit(1);
               return;
         }
      }

      for current_placeholder, name: placeholders {
         if !current_placeholder.count {
            color_print("@yellow,bold{.. warning} empty placeholder: @bold{%}\n", name, stderr = true);
            continue;
         }
      }
   }

   { // Parse the source file
      file, ok := read_entire_file(input_filename);
      if !ok {
         color_print("@red,bold{.. error} unable to open file: @bold{%}\n", input_filename, stderr = true);
         exit(1);
         return;
      }

      builder: String_Builder;

      scan := file;
      while scan.count {
         chr, ok, width := consume_rune(*scan);
         if !ok break;

         // escaped '@'
         if chr == #char "\\" {
            next, ok := peek_rune(scan);
            if ok && next == #char "@" {
               consume_rune(*scan);
               append(*builder, next);
               continue;
            }
         }
         // placeholder
         else if chr == #char "@" {
            view := begin_view(scan);

            // scan the name
            while scan.count {
               chr, ok, width := peek_rune(scan);
               if !ok || !valid_char(chr) break;

               advance_view(*view, width);
               consume_rune(*scan);
            }

            name := end_view(view);

            field: string;
            append_dot := false;

            // scan a field if we find one
            chr, ok, width = peek_rune(scan);
            if ok && chr == #char "." {
               consume_rune(*scan);

               // only scan the field if there's something
               // valid after the '.', otherwise it's just
               // a '.' by itself.
               chr, ok, width = peek_rune(scan);

               append_dot = !ok || !valid_char(chr);
               if !append_dot {
                  view := begin_view(scan);
                  while scan.count {
                     chr, ok, width = peek_rune(scan);
                     if !ok || !valid_char(chr) break;

                     advance_view(*view, width);
                     consume_rune(*scan);
                  }

                  field = end_view(view);
               }
            }

            placeholder, ok := table_find(*placeholders, name);
            if !ok {
               color_print("@red,bold{.. warning} unknown placeholder: @bold{%}\n", name, stderr = true);
               continue;
            }

            value: string;

            if field.count {
               field_value, ok := table_find(placeholder, field);
               if !ok {
                  color_print("@red{.. unknown field} %.@bold{%}\n", name, field, stderr = true);
                  exit(1);
                  return;
               }

               value = field_value;
            }
            else {
               given_name, has_name := table_find(placeholder, placeholder.default_field);
               value = ifx has_name then given_name else name;
            }

            append(*builder, value);
            if append_dot append(*builder, #char ".");

            continue;
         }

         append(*builder, chr);
      }

      // Print the new output to stdout
      write_strings(to_string(*builder), "\n");
   }
}

valid_char :: (r: rune) -> bool {
   return (r >= #char "a" && r <= #char "z") ||
          (r >= #char "A" && r <= #char "Z") ||
          (r >= #char "0" && r <= #char "9") ||
          (r == #char "_" || r == #char "-");
}

Placeholder_Map :: struct {
   using fields: Table(string, string);
   default_field := "name";
}

#import "File";
#import "Basic";
#import "String";
#import "Hash_Table";
#import,file "./base/module.jai";

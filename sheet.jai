/*
   Sheet is a tool to replace parts of source
   text with values defined in a definition
   sheet; sheet file for short, hence the name

   For example, when writing a story, the names
   of characters/places/things may not be final
   and may need to be changed frequently. Sheet
   attempts to solve this problem by replacing
   special text within the source with values
   defined in the sheet file.

   The basic syntax of a sheet file:
      @name
         .name Default Name
         .field Value of field
         .another Field with a value
         .one-more_time 100

   Usage within the source text:
      Lorem @name dolor sit amet,
      consectetur adipisicing elit, sed do
      eiusmod @name.field.

   After calling 'sheet <input> <sheet>'
      Lorem Default Name dolor sit amet,
      consectetur adipisicing elit, sed do
      eiusmod Value of field.

   On success, sheet will write the converted
   text to stdout.
*/

// @Todo:
// - Allow escaped '@' in the source text
// - Allow comments to be placed anywhere in the sheet
// - Add unicode support (not hard, just lazy)

PROGRAM_NAME    :: "j-sheet";
PROGRAM_VERSION :: "1a";
PROGRAM_USAGE   :: "@bold{.. usage} % @bold{<input file>} @bold{<sheet file>}\n";

#run ct_compile_to_binary(PROGRAM_NAME);

main :: () {
   cl_args := get_command_line_arguments();
   defer free(cl_args.data);

   input_filename: string;
   sheet_filename: string;

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
            color_print("   @bold{input} a file containing character @ul{sequences}\n", stderr = true);
            color_print("   @bold{sheet} a file containing character @ul{definitions}\n", stderr = true);
            return;

         case "-v"; #through;
         case "-version"; #through;
         case "--version";
            color_print("@bold{.. %} @green{v%}\n", PROGRAM_NAME, PROGRAM_VERSION, stderr = true);
            return;
      }

      input_filename = args[0];

      args.count -= 1;
      args.data  += 1;

      if !args.count {
         color_print("@red,bold{.. error} expected sheet file!\n", stderr = true);
         return;
      }

      sheet_filename = args[0];
   }

   source, ok := read_entire_file(sheet_filename);
   if !ok {
      color_print("@red,bold{.. error} unable to open file: @bold{%}\n", sheet_filename, stderr = true);
      return;
   }

   values: *Character_Values;
   characters: Table(string, *Character_Values);

   { // Parse the sheet file
      lines := split(source, "\n");
      for _: lines {
         it := trim(_);
         if !it.count continue;

         // Skip comments
         if it.count > 1       &&
            it[0] == #char "-" &&
            it[1] == #char "-" then continue;

         // Starts a new character
         if it[0] == #char "@" {
            name := it;
            name.count -= 1;
            name.data  += 1;
            name = trim(name);

            values = New(Character_Values);
            table_add(*characters, name, values);
            continue;
         }

         line := trim(it);
         if line.count <= 0 continue;

         // A value belonging to a character
         if line[0] == #char "." {
            name := line;
            name.count -= 1;
            name.data  += 1;

            for 0..name.count - 1 {
               if name[it] == #char " " {
                  name.count = it;
                  break;
               }
            }

            line.data  += name.count + 1;
            line.count -= name.count + 1;
            line = trim(line);

            if !line.count {
               color_print("@red,bold{.. error} invalid line!\n   @bold{%}\n", it, stderr = true);
               return;
            }

            if !values {
               color_print("@red,bold{.. error} value without character!\n   @bold{%}\n", it, stderr = true);
               return;
            }

            table_add(values, name, line);
            continue;
         }
      }

      for values, name: characters {
         if !values.count {
            color_print("@yellow,bold{.. warning} empty character: @bold{%}\n", name, stderr = true);
            continue;
         }
      }
   }

   { // Parse the source file
      file, ok := read_entire_file(input_filename);
      if !ok {
         color_print("@red,bold{.. error} unable to open file: @bold{%}\n", input_filename, stderr = true);
         return;
      }

      builder: String_Builder;

      valid_char :: (r: rune) -> bool {
         return (r >= #char "a" && r <= #char "z") ||
                (r >= #char "A" && r <= #char "Z") ||
                (r >= #char "0" && r <= #char "9") ||
                (r == #char "_" || r == #char "-");
      }

      scan := file;
      while scan.count {
         chr := scan.data[0];
         scan.data  += 1;
         scan.count -= 1;

         // character
         if chr == #char "@" {
            field: string;

            name := scan;
            name.count = 0;

            // scan the name
            while scan.count {
               chr = scan.data[0];
               if !valid_char(chr) {
                  // scan the field
                  if chr == #char "." {
                     scan.data  += 1;
                     scan.count -= 1;

                     field.data = scan.data;

                     while scan.count {
                        chr = scan.data[0];
                        if !valid_char(chr) break;

                        field.count += 1;
                        scan.data   += 1;
                        scan.count  -= 1;
                     }

                     break;
                  }

                  break;
               }

               name.count += 1;
               scan.data  += 1;
               scan.count -= 1;
            }

            character, ok := table_find(*characters, name);
            if !ok {
               color_print("@red,bold{.. error} unknown character: @bold{%}\n", name, stderr = true);
               continue;
            }

            value: string;

            if field.count {
               field_value, ok := table_find(character, field);
               if !ok {
                  color_print("@red{.. unknown field} %.@bold{%}\n", name, field, stderr = true);
                  return;
               }

               value = field_value;
            }
            else {
               given_name, has_name := table_find(character, "name");
               value = ifx has_name then given_name else name;
            }

            append(*builder, value);
            continue;
         }

         append(*builder, chr);
      }

      // extension, has_extension := path_extension(input_filename);
      // output_path := path_strip_extension(input_filename);

      // if has_extension then extension = tprint(".%", extension);
      // output_filename := tprint("%-compiled%", output_path, extension);

      // Print the new output to stdout
      write_strings(to_string(*builder), "\n");
   }
}

Character_Values :: Table(string, string);

#if OS == .WINDOWS {
   ENDING :: "\r\n";
} else {
   ENDING :: "\n";
}

#import "File";
#import "Basic";
#import "String";
#import "Hash_Table";
#import,file "./base/module.jai";

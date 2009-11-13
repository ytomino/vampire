-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Directories;
with DYAYaml;
with Tabula.Configurations;
with Tabula.File_IO;
with Tabula.Users.User_Info_IO;
procedure Tabula.Users.Load (Id : in String; User_Info : in out Users.User_Info) is
	File_Name : String renames Ada.Directories.Compose (Tabula.Configurations.Users_Directory, Id);
	S : String renames File_IO.Read_File (File_Name);
begin
	declare
		Reader : DYAYaml.Reader := DYAYaml.New_Reader (Users.User_Info_IO.Yaml_Type, S);
	begin
		Users.User_Info_IO.IO(Reader, User_Info);
	end;
exception
	when DYAYaml.YAML_Error =>
		raise DYAYaml.YAML_Error with File_Name;
end Tabula.Users.Load;

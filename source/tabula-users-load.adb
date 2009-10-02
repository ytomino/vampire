-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Directories;
with DYAYaml;
with Tabula.Configurations;
with Tabula.File_IO;
with Tabula.Users.User_Info_IO;
procedure Tabula.Users.Load(Id : String; User_Info : in out Users.User_Info) is
	File_Name : String renames Ada.Directories.Compose(Tabula.Configurations.Users_Directory, Id);
	Reader : DYAYaml.Reader := DYAYaml.New_Reader(
		Users.User_Info_IO.Yaml_Type,
		File_IO.Read_File(File_Name));
begin
	Users.User_Info_IO.IO(Reader, User_Info);
end Tabula.Users.Load;

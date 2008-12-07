-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Directories;
with Ase.Directories;
with DYAYaml;
with Tabula.Configurations;
with Tabula.Users.User_Info_IO;
procedure Tabula.Users.Save(Id : String; User_Info : in out Users.User_Info) is
	File_Name : String renames Ada.Directories.Compose(Tabula.Configurations.Users_Directory, Id);
	Writer : DYAYaml.Writer := DYAYaml.New_Writer(Users.User_Info_IO.Yaml_Type);
begin
	Users.User_Info_IO.IO(Writer, User_Info);
	Ase.Directories.Write_File(File_Name, DYAYaml.Output(Writer));
end Tabula.Users.Save;

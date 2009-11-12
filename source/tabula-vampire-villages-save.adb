-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Directories;
with DYAYaml;
with Tabula.Configurations;
with Tabula.File_IO;
with Tabula.Vampire.Villages.Village_IO;
procedure Tabula.Vampire.Villages.Save (
	Id : Tabula.Villages.Lists.Village_Id;
	Village : in out Villages.Village_Type)
is
	File_Name : String renames Ada.Directories.Compose(Tabula.Configurations.Villages_Data_Directory, Id);
	Writer : DYAYaml.Writer := DYAYaml.New_Writer(Villages.Village_IO.Yaml_Type);
begin
	Villages.Village_IO.IO(Writer, Village);
	File_IO.Write_File(File_Name, DYAYaml.Output(Writer));
end Tabula.Vampire.Villages.Save;

-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Directories;
with Ase.Directories;
with DYAYaml;
with Tabula.Configurations;
with Tabula.Villages.Village_IO;
procedure Tabula.Villages.Save(Id : Lists.Village_Id; Village : in out Village_Type) is
	File_Name : String renames Ada.Directories.Compose(Tabula.Configurations.Villages_Data_Directory, Id);
	Writer : DYAYaml.Writer := DYAYaml.New_Writer(Villages.Village_IO.Yaml_Type);
begin
	Villages.Village_IO.IO(Writer, Village);
	Ase.Directories.Write_File(File_Name, DYAYaml.Output(Writer));
end Tabula.Villages.Save;

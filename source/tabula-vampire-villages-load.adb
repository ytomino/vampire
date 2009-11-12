-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Directories;
with DYAYaml;
with Tabula.Configurations;
with Tabula.File_IO;
with Tabula.Villages.Village_IO;
procedure Tabula.Villages.Load(Id : Lists.Village_Id; Village : in out Village_Type; Info_Only : Boolean := False) is
	File_Name : String renames Ada.Directories.Compose(Tabula.Configurations.Villages_Data_Directory, Id);
	Reader : DYAYaml.Reader := DYAYaml.New_Reader(
		Villages.Village_IO.Yaml_Type,
		File_IO.Read_File(File_Name));
begin
	Villages.Village_IO.IO(Reader, Village, Info_Only);
end Tabula.Villages.Load;

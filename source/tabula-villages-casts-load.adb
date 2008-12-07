-- The Village of Vampire by YT, このソースコードはNYSLです
with Ase.Directories;
with DYAYaml;
with Tabula.Configurations;
with Tabula.Villages.Casts.Cast_IO;
procedure Tabula.Villages.Casts.Load(Cast : in out Cast_Type) is
	Reader : DYAYaml.Reader := DYAYaml.New_Reader(
		Villages.Casts.Cast_IO.Yaml_Type, 
		Ase.Directories.Read_File(Tabula.Configurations.Cast_File_Name));
begin
	Cast_IO.IO(Reader, Cast);
end Tabula.Villages.Casts.Load;

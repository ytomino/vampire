-- The Village of Vampire by YT, このソースコードはNYSLです
with DYAYaml;
with Tabula.Configurations;
with Tabula.File_IO;
with Tabula.Villages.Casts.Cast_IO;
procedure Tabula.Villages.Casts.Load(Cast : in out Cast_Type) is
	Reader : DYAYaml.Reader := DYAYaml.New_Reader(
		Villages.Casts.Cast_IO.Yaml_Type, 
		Tabula.File_IO.Read_File(Tabula.Configurations.Cast_File_Name));
begin
	Cast_IO.IO(Reader, Cast);
end Tabula.Villages.Casts.Load;

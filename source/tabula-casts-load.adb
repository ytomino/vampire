-- The Village of Vampire by YT, このソースコードはNYSLです
with DYAYaml;
with Tabula.Configurations;
with Tabula.File_IO;
with Tabula.Casts.Cast_IO;
procedure Tabula.Casts.Load (Cast : in out Cast_Collection) is
	Reader : DYAYaml.Reader := DYAYaml.New_Reader (
		Casts.Cast_IO.Yaml_Type, 
		Tabula.File_IO.Read_File(Tabula.Configurations.Cast_File_Name));
begin
	Cast_IO.IO (Reader, Cast);
end Tabula.Casts.Load;

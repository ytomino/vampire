-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Streams.Stream_IO;
with Serialization.YAML;
with YAML.Streams;
with Tabula.Configurations;
with Tabula.Casts.Cast_IO;
function Tabula.Casts.Load return Cast_Collection is
begin
	return Result : Cast_Collection do
		declare
			File : Ada.Streams.Stream_IO.File_Type;
		begin
			Ada.Streams.Stream_IO.Open (
				File,
				Ada.Streams.Stream_IO.In_File,
				Name => Tabula.Configurations.Cast_File_Name);
			declare
				Parser : aliased YAML.Parser := YAML.Streams.Create (
					Ada.Streams.Stream_IO.Stream (File));
			begin
				YAML.Parse_Stream_Start (Parser);
				Cast_IO.IO (
					Serialization.YAML.Reading (Parser'Access, Cast_IO.Yaml_Type).Serializer,
					Result);
				YAML.Parse_Stream_End (Parser);
			end;
			Ada.Streams.Stream_IO.Close (File);
		end;
	end return;
end Tabula.Casts.Load;

-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Streams.Stream_IO;
with Serialization.YAML;
with YAML.Streams;
with Tabula.Casts.Cast_IO;
function Tabula.Casts.Load (Name : String) return Cast_Collection is
	Result : Cast_Collection;
begin
--	return Result : Cast_Collection do -- [gcc-4.7] bug of compiler
		declare
			File : Ada.Streams.Stream_IO.File_Type :=
				Ada.Streams.Stream_IO.Open (Ada.Streams.Stream_IO.In_File, Name => Name);
		begin
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
--	end return; -- [gcc-4.7] bug of compiler
	return Result;
end Tabula.Casts.Load;

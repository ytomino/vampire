-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Serialization.YAML;
with YAML.Streams;
with Tabula.Configurations;
with Tabula.Vampires.Villages.Village_IO;
procedure Tabula.Vampires.Villages.Load (
	Id : in Tabula.Villages.Village_Id;
	Village : in out Village_Type;
	Info_Only : in Boolean := False)
is
	File_Name : String renames Ada.Directories.Compose (
		Tabula.Configurations.Villages_Data_Directory,
		Id);
	File : Ada.Streams.Stream_IO.File_Type;
begin
	Ada.Streams.Stream_IO.Open (
		File,
		Ada.Streams.Stream_IO.In_File,
		Name => File_Name);
	declare
		Parser : aliased YAML.Parser := YAML.Streams.Create (
			Ada.Streams.Stream_IO.Stream (File));
	begin
		YAML.Parse_Stream_Start (Parser);
		Village_IO.IO (
			Serialization.YAML.Reading (Parser'Access, Village_IO.Yaml_Type).Serializer,
			Village,
			Info_Only);
		YAML.Parse_Stream_End (Parser);
	end;
	Ada.Streams.Stream_IO.Close (File);
end Tabula.Vampires.Villages.Load;

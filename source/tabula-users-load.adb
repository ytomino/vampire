-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Serialization.YAML;
with YAML.Streams;
with Tabula.Configurations;
with Tabula.Users.User_Info_IO;
procedure Tabula.Users.Load (
	Id : in String;
	User_Info : in out Users.User_Info)
is
	File_Name : String renames Ada.Directories.Compose (
		Tabula.Configurations.Users_Directory,
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
		User_Info_IO.IO (
			Serialization.YAML.Reading (Parser'Access, User_Info_IO.Yaml_Type).Serializer,
			User_Info);
		YAML.Parse_Stream_End (Parser);
	end;
	Ada.Streams.Stream_IO.Close (File);
end Tabula.Users.Load;

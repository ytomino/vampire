-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Exceptions;
with Ada.Streams.Stream_IO;
with Serialization.YAML;
with YAML.Streams;
with Vampire.Villages.Village_IO;
procedure Vampire.Villages.Load (
	Name : in String;
	Village : in out Village_Type;
	Info_Only : in Boolean := False)
is
	File : Ada.Streams.Stream_IO.File_Type;
begin
	Ada.Streams.Stream_IO.Open (
		File,
		Ada.Streams.Stream_IO.In_File,
		Name => Name);
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
exception
	when E : others =>
		declare
			Message : constant String := Name & ": " & Ada.Exceptions.Exception_Message (E);
		begin
			Ada.Debug.Put (Message);
			Ada.Exceptions.Raise_Exception (
				Ada.Exceptions.Exception_Identity (E),
				Message);
		end;
end Vampire.Villages.Load;

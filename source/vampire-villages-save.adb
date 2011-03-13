-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Exceptions;
with Ada.Streams.Stream_IO;
with Serialization.YAML;
with YAML.Streams;
with Vampire.Villages.Village_IO;
procedure Vampire.Villages.Save (
	Name : in String;
	Village : in out Villages.Village_Type)
is
	File : Ada.Streams.Stream_IO.File_Type;
begin
	Ada.Streams.Stream_IO.Create (
		File,
		Name => Name);
	declare
		Emitter : aliased YAML.Emitter := YAML.Streams.Create (
			Ada.Streams.Stream_IO.Stream (File));
	begin
		YAML.Set_Unicode (Emitter, True);
		YAML.Emit (Emitter, (Event_Type => YAML.Stream_Start, Encoding => YAML.UTF_8));
		Village_IO.IO (
			Serialization.YAML.Writing (Emitter'Access, Village_IO.Yaml_Type).Serializer,
			Village);
		YAML.Emit (Emitter, (Event_Type => YAML.Stream_End));
		YAML.Flush (Emitter);
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
end Vampire.Villages.Save;

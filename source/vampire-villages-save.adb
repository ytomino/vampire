-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Exceptions;
with Ada.Directories.Temporary;
with Ada.Streams.Stream_IO;
with Serialization.YAML;
with YAML.Streams;
with Vampire.Villages.Village_IO;
procedure Vampire.Villages.Save (
	Name : in String;
	Village : in out Villages.Village_Type)
is
	Temporary_Name : constant String := Ada.Directories.Temporary.Create_Temporary_File;
	File : Ada.Streams.Stream_IO.File_Type := Ada.Streams.Stream_IO.Create (Name => Temporary_Name);
begin
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
	Ada.Directories.Replace_File (Source_Name => Temporary_Name, Target_Name => Name);
exception
	when E : others =>
		Ada.Debug.Put (Temporary_Name);
		declare
			Message : constant String := Name & ": " & Ada.Exceptions.Exception_Message (E);
		begin
			Ada.Debug.Put (Message);
			Ada.Exceptions.Raise_Exception (
				Ada.Exceptions.Exception_Identity (E),
				Message);
		end;
end Vampire.Villages.Save;

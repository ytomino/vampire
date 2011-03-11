-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Streams.Stream_IO;
with Serialization.YAML;
with YAML.Streams;
with Tabula.Users.User_Info_IO;
procedure Tabula.Users.Save (
	Name : in String;
	Info : in out User_Info)
is
	File : Ada.Streams.Stream_IO.File_Type := Ada.Streams.Stream_IO.Create (Name => Name);
begin
	declare
		Emitter : aliased YAML.Emitter := YAML.Streams.Create (
			Ada.Streams.Stream_IO.Stream (File));
	begin
		YAML.Set_Unicode (Emitter, True);
		YAML.Emit (Emitter, (Event_Type => YAML.Stream_Start, Encoding => YAML.UTF_8));
		User_Info_IO.IO (
			Serialization.YAML.Writing (Emitter'Access, User_Info_IO.Yaml_Type).Serializer,
			Info);
		YAML.Emit (Emitter, (Event_Type => YAML.Stream_End));
		YAML.Flush (Emitter);
	end;
	Ada.Streams.Stream_IO.Close (File);
end Tabula.Users.Save;

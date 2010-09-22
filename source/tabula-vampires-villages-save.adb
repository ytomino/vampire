-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Serialization.YAML;
with YAML.Streams;
with Tabula.Configurations;
with Tabula.Vampires.Villages.Village_IO;
procedure Tabula.Vampires.Villages.Save (
	Id : in Tabula.Villages.Village_Id;
	Village : in out Villages.Village_Type)
is
	File_Name : String renames Ada.Directories.Compose(
		Tabula.Configurations.Villages_Data_Directory,
		Id);
	File : Ada.Streams.Stream_IO.File_Type;
begin
	Ada.Streams.Stream_IO.Create (
		File,
		Name => File_Name);
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
end Tabula.Vampires.Villages.Save;

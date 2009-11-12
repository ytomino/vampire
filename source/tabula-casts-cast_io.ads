-- The Village of Vampire by YT, このソースコードはNYSLです
with DYAYaml;
package Tabula.Casts.Cast_IO is
	
	Default_Person : constant Person := (
		Name => Ada.Strings.Unbounded.Null_Unbounded_String,
		Work => Ada.Strings.Unbounded.Null_Unbounded_String,
		Image => Ada.Strings.Unbounded.Null_Unbounded_String,
		Sex => Casts.Male,
		Group => 0);
	
	Default_Work : constant Work := (
		Name => Ada.Strings.Unbounded.Null_Unbounded_String,
		Sex => Neutral,
		Nominated => False);
	
	Yaml_Type : constant String := "vampire-cast";
	
	package Sex_Kind_IO is new DYAYaml.IO_Enumeration(Sex_Kind);
	package Person_Sex_IO is new DYAYaml.IO_Enumeration(Person_Sex);
	
	procedure IO (Serializer : in out DYAYaml.Serializer; Item : in out Person'Class);
	procedure IO (Serializer : in out DYAYaml.Serializer; Item : in out Cast_Collection);
	
end Tabula.Casts.Cast_IO;

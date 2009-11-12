-- The Village of Vampire by YT, このソースコードはNYSLです
with DYAYaml;
package Tabula.Casts.Cast_IO is
	
	Yaml_Type : constant String := "vampire-cast";
	
	package Sex_Kind_IO is new DYAYaml.IO_Enumeration(Sex_Kind);
	package Person_Sex_IO is new DYAYaml.IO_Enumeration(Person_Sex);
	
	procedure IO (Serializer : in out DYAYaml.Serializer; Item : in out Person'Class);
	procedure IO (Serializer : in out DYAYaml.Serializer; Item : in out Cast_Collection);
	
end Tabula.Casts.Cast_IO;

-- The Village of Vampire by YT, このソースコードはNYSLです
with DYAYaml;
package Tabula.Villages.Casts.Cast_IO is
	
	Yaml_Type : constant String := "vampire-cast";
	
	procedure IO(Serializer: in out DYAYaml.Serializer; Cast: in out Cast_Type);
	
end Tabula.Villages.Casts.Cast_IO;

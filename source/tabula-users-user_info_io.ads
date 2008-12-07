-- The Village of Vampire by YT, このソースコードはNYSLです
with DYAYaml;
package Tabula.Users.User_Info_IO is
	
	Yaml_Type : constant String := "vampire-user";
	
	procedure IO(Serializer: in out DYAYaml.Serializer; Value: in out User_Info);
	
end Tabula.Users.User_Info_IO;

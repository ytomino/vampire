-- The Village of Vampire by YT, このソースコードはNYSLです
with Serialization;
package Tabula.Users.User_Info_IO is
	
	Yaml_Type : constant String := "vampire-user";
	
	procedure IO (Serializer : not null access Serialization.Serializer; Value : in out User_Info);
	
end Tabula.Users.User_Info_IO;

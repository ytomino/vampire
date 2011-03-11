-- The Village of Vampire by YT, このソースコードはNYSLです
with Serialization;
package Tabula.Users.User_Info_IO is
	
	Yaml_Type : constant String := "vampire-user";
	
	procedure IO (
		Serializer : not null access Serialization.Serializer;
		Value : in out User_Info);
	
	package Password_Digest_IO is new Serialization.IO_Custom (
		Password_Digest,
		Image,
		Value);
	
end Tabula.Users.User_Info_IO;

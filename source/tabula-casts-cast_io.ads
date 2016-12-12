-- The Village of Vampire by YT, このソースコードはNYSLです
with Serialization;
package Tabula.Casts.Cast_IO is
	
	Yaml_Type : constant String := "vampire-cast";
	
	package Neutralable_Sex_IO is
		new Serialization.IO_Enumeration (Neutralable_Sex);
	package Person_Sex_IO is new Serialization.IO_Enumeration (Person_Sex);
	
	procedure IO_Partial (
		Serializer : not null access Serialization.Serializer;
		Item : in out Person'Class);
	procedure IO (
		Serializer : not null access Serialization.Serializer;
		Item : in out Cast_Collection);
	
end Tabula.Casts.Cast_IO;

-- The Village of Vampire by YT, このソースコードはNYSLです
with Serialization;
package Tabula.Calendar.Time_IO is
	
	procedure IO (
		Serializer : not null access Serialization.Serializer;
		Name : in String;
		Value : in out Ada.Calendar.Time);
	procedure IO (
		Serializer : not null access Serialization.Serializer;
		Name : in String;
		Value : in out Duration);
	
end Tabula.Calendar.Time_IO;

-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Calendar.Formatting;
package body Tabula.Calendar.Time_IO is
	
	function Image (Date : Ada.Calendar.Time) return String is
	begin
		return Ada.Calendar.Formatting.Image (Date, Time_Zone => Time_Offset);
	end Image;
	
	function Value (Image : String) return Ada.Calendar.Time is
	begin
		return Ada.Calendar.Formatting.Value (Image, Time_Zone => Time_Offset);
	end Value;
	
	package Time_IO is
		new Serialization.IO_Custom (
			Ada.Calendar.Time,
			Image,
			Value,
			Triming => False);
	procedure IO (
		Serializer : not null access Serialization.Serializer;
		Name : in String;
		Value : in out Ada.Calendar.Time)
		renames Time_IO.IO;

	package Duration_IO is
		new Serialization.IO_Custom (
			Duration,
			Duration'Image,
			Duration'Value,
			Triming => True);
	procedure IO (
		Serializer : not null access Serialization.Serializer;
		Name : in String;
		Value : in out Duration)
		renames Duration_IO.IO;
	
end Tabula.Calendar.Time_IO;

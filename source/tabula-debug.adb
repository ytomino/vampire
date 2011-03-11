-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Calendar.Formatting;
with Ada.Text_IO;
with Tabula.Calendar;
with System.Debug;
package body Tabula.Debug is
	
	Name : Static_String_Access;
	File : Ada.Text_IO.File_Type;
	Time : Ada.Calendar.Time;
	
	procedure Start is
		Time_Image : constant String := Ada.Calendar.Formatting.Image (Time, Time_Zone => Tabula.Calendar.Time_Offset);
		Offset_Image : constant String := Tabula.Calendar.Image (Tabula.Calendar.Time_Offset);
	begin
		if Name = null then
			raise Program_Error with "debug log handler is not installed.";
		end if;
		Ada.Text_IO.Open (File, Ada.Text_IO.Append_File, Name.all);
		Ada.Text_IO.Put (File, "---- " & Time_Image & " (" & Offset_Image & ") ----");
		Ada.Text_IO.New_Line (File);
	end Start;
	
	function Put (
		S : String;
		Source_Location : String;
		Enclosing_Entity : String)
		return Boolean is
	begin
		if not Ada.Text_IO.Is_Open (File) then
			Start;
		end if;
		Ada.Text_IO.Put (File, Source_Location & ": (" & Enclosing_Entity & ") " & S);
		Ada.Text_IO.New_Line (File);
		return True;
	end Put;
	
	procedure Hook (Name : not null Static_String_Access) is
	begin
		Debug.Name := Name;
		System.Debug.Put_Hook := Put'Access;
	end Hook;
	
end Tabula.Debug;

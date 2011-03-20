-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Calendar.Formatting;
with Ada.Streams.Stream_IO;
with Tabula.Calendar;
with System.Debug;
package body Tabula.Debug is
	
	Name : Static_String_Access;
	File : Ada.Streams.Stream_IO.File_Type;
	Time : Ada.Calendar.Time;
	
	procedure Start is
		Time_Image : constant String := Ada.Calendar.Formatting.Image (Time, Time_Zone => Tabula.Calendar.Time_Offset);
		Offset_Image : constant String := Ada.Calendar.Formatting.Image (Tabula.Calendar.Time_Offset);
	begin
		if Name = null then
			raise Program_Error with "debug log handler is not installed.";
		end if;
		Ada.Streams.Stream_IO.Create (File, Ada.Streams.Stream_IO.Append_File, Name.all);
		declare
			Stream : Ada.Streams.Stream_IO.Stream_Access :=
				Ada.Streams.Stream_IO.Stream (File);
		begin
			String'Write (Stream, "---- " & Time_Image & " (GMT" & Offset_Image & ") ----" & ASCII.LF);
		end;
	end Start;
	
	function Put (
		S : String;
		Source_Location : String;
		Enclosing_Entity : String)
		return Boolean is
	begin
		if not Ada.Streams.Stream_IO.Is_Open (File) then
			Start;
		end if;
		declare
			Stream : Ada.Streams.Stream_IO.Stream_Access :=
				Ada.Streams.Stream_IO.Stream (File);
		begin
			String'Write (Stream, Source_Location & ": (" & Enclosing_Entity & ") " & S & ASCII.LF);
		end;
		return True;
	end Put;
	
	procedure Hook (Name : not null Static_String_Access; Time : Ada.Calendar.Time) is
	begin
		Debug.Name := Name;
		Debug.Time := Time;
		System.Debug.Put_Hook := Put'Access;
	end Hook;
	
end Tabula.Debug;

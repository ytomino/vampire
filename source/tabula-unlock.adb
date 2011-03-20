-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Calendar;
with Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Streams.Stream_IO.Standards;
with Web;
with Tabula.Debug;
procedure Tabula.Unlock (
	Lock_Name : in not null Static_String_Access;
	Debug_Log_File_Name : in not null Static_String_Access)
is
	Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
	
	Output : not null Ada.Streams.Stream_IO.Stream_Access :=
		Ada.Streams.Stream_IO.Stream (Ada.Streams.Stream_IO.Standards.Standard_Output.all);
	
	Count : Natural := 0;
begin
	Debug.Hook (Debug_Log_File_Name, Now);
	Deleting: loop
		begin
			Ada.Directories.Delete_Tree (Lock_Name.all);
			delay 0.1;
			Count := Count + 1;
		exception
			when Ada.IO_Exceptions.Name_Error => exit Deleting;
		end;
	end loop Deleting;
	declare
		Message : constant String := Natural'Image (Count) & " OK.";
	begin
		Ada.Debug.Put (Message);
		Web.Header_Content_Type (Output, Web.Text_Plain);
		Web.Header_Break(Output);
		String'Write (Output, Message);
		Character'Write (Output, ASCII.LF);
	end;
end Tabula.Unlock;

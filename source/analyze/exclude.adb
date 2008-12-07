with Ada.Calendar.Formatting;
with Ada.Command_Line;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with GNAT.Regexp;
procedure exclude is
	use type Ada.Strings.Unbounded.Unbounded_String;
	use type Ada.Calendar.Time;
	function "+" (S : Ada.Strings.Unbounded.Unbounded_String) return String renames Ada.Strings.Unbounded.To_String;
	type User_Log_Item is record
		Id : Ada.Strings.Unbounded.Unbounded_String;
		Remote_Addr : Ada.Strings.Unbounded.Unbounded_String;
		Remote_Host : Ada.Strings.Unbounded.Unbounded_String;
	end record;
	function "<" (Left, Right : User_Log_Item) return Boolean is
	begin
		if Left.Id < Right.Id then
			return True;
		elsif Left.Id > Right.Id then
			return False;
		elsif Left.Remote_Addr < Right.Remote_Addr then
			return True;
		elsif Left.Remote_Addr > Right.Remote_Addr then
			return False;
		else
			return Left.Remote_Host < Right.Remote_Host;
		end if;
	end "<";
	package Users_Log is new Ada.Containers.Indefinite_Ordered_Maps(User_Log_Item, Ada.Calendar.Time);
	use Users_Log;
	Users_Log_File_Name : constant String := Ada.Command_Line.Argument(1);
	Excluding : constant String := Ada.Command_Line.Argument(2);
	File : Ada.Streams.Stream_IO.File_Type;
	Log : Users_Log.Map;
	use GNAT.Regexp;
	Pattern : Regexp;
	use Ada.Text_IO;
begin
	Pattern := Compile(Excluding, Glob => True);
	Ada.Streams.Stream_IO.Open(File, Ada.Streams.Stream_IO.In_File, Users_Log_File_Name);
	Users_Log.Map'Read(Ada.Streams.Stream_IO.Stream(File), Log);
	Ada.Streams.Stream_IO.Close(File);
	declare
		I : Users_Log.Cursor := First(Log);
		N : Users_Log.Cursor;
	begin
		while Has_Element(I) loop
			N := Next(I);
			if Match(+Key(I).Id, Pattern) or else Match(+Key(I).Remote_Host, Pattern) then
				Delete(Log, I);
			end if;
			I := N;
		end loop;
	end;
	Ada.Streams.Stream_IO.Create(File, Ada.Streams.Stream_IO.Out_File, Users_Log_File_Name);
	Users_Log.Map'Write(Ada.Streams.Stream_IO.Stream(File), Log);
	Ada.Streams.Stream_IO.Close(File);
end exclude;

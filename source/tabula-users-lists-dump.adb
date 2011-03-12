with Ada.Calendar.Formatting;
with Ada.Command_Line;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
procedure Analyze is
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
	File : Ada.Streams.Stream_IO.File_Type;
	Log : Users_Log.Map;
	use Ada.Text_IO;
begin
	Ada.Streams.Stream_IO.Open(File, Ada.Streams.Stream_IO.In_File, Users_Log_File_Name);
	Users_Log.Map'Read(Ada.Streams.Stream_IO.Stream(File), Log);
	Ada.Streams.Stream_IO.Close(File);
	declare
		I : Users_Log.Cursor := First(Log);
	begin
		while Has_Element(I) loop
			Put(+Key(I).Id);
			Put(',');
			Put(+Key(I).Remote_Addr);
			Put(',');
			Put(+Key(I).Remote_Host);
			Put(',');
			Put(Ada.Calendar.Formatting.Image(Element(I)));
			New_Line;
			Next(I);
		end loop;
	end;
	New_Line;
	declare
		I : Users_Log.Cursor := First(Log);
	begin
		while Has_Element(I) loop
			declare
				J : Users_Log.Cursor := Next(I);
			begin
				while Has_Element(J) loop
					if Key(I).Remote_Addr = Key(J).Remote_Addr 
						or else (Key(I).Remote_Host = Key(J).Remote_Host and then Key(J).Remote_Host /= "")
					then
						Put(+Key(I).Id);
						Put(',');
						Put(+Key(I).Remote_Addr);
						Put(',');
						Put(+Key(I).Remote_Host);
						Put(',');
						Put(Ada.Calendar.Formatting.Image(Element(I)));
						New_Line;
						Put(+Key(J).Id);
						Put(',');
						Put(+Key(J).Remote_Addr);
						Put(',');
						Put(+Key(J).Remote_Host);
						Put(',');
						Put(Ada.Calendar.Formatting.Image(Element(J)));
						New_Line;
					end if;
					Next(J);
				end loop;
			end;
			Next(I);
		end loop;
	end;
end Analyze;

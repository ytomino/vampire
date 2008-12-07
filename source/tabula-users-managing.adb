-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;
with Tabula.Configurations;
with Tabula.Users.Load;
with Tabula.Users.Save;
package body Tabula.Users.Managing is
	
	-- function "+" (S : Ada.Strings.Unbounded.Unbounded_String) return String renames Ada.Strings.Unbounded.To_String;
	function "+" (S : String) return Ada.Strings.Unbounded.Unbounded_String renames Ada.Strings.Unbounded.To_Unbounded_String;
	
	procedure Check(Id, Password : String; 
		Remote_Addr, Remote_Host : in String;
		Now : in Ada.Calendar.Time;
		Result : out Check_Result; 
		User_Info : out Users.User_Info) 
	is
		use type Ada.Strings.Unbounded.Unbounded_String;
	begin
		if Id = "" then
			Result := Log_Off;
		elsif not Ada.Directories.Exists(Ada.Directories.Compose(Configurations.Users_Directory, Id)) then
			Result := Unknown;
		else
			Users.Load(Id, User_Info);
			if User_Info.Password /= Digest(Password) 
				or else User_Info.Renamed /= Ada.Strings.Unbounded.Null_Unbounded_String
			then
				Result := Invalid;
			else
				Result := Valid;
				if Id /= Tabula.Users.Administrator then
					if not User_Info.No_Log then
						Add_To_Users_Log(
							User_Id => Id, 
							Remote_Addr => Remote_Addr, 
							Remote_Host => Remote_Host,
							Time => Now);
					else
						Add_To_Users_Log(User_Id => Id, Remote_Addr => "", Remote_Host => "", Time => Now);
					end if;
				end if;
			end if;
		end if;
	end Check;
	
	procedure New_User(Id, Password : in String;
		Remote_Addr, Remote_Host : in String;
		Now : in Ada.Calendar.Time;
		Result : out Boolean) is
	begin
		if Users.Managing.Exists(Id) then
			Result := False;
		else
			declare
				User_Info : Users.User_Info := (
					Password => +Users.Digest(Password),
					Remote_Addr => +Remote_Addr,
					Remote_Host => +Remote_Host,
					Creation_Time => Now,
					Last_Remote_Addr => +Remote_Addr,
					Last_Remote_Host => +Remote_Host,
					Last_Time => Now,
					Ignore_Request => False,
					Disallow_New_Village => False,
					No_Log => False,
					Renamed => Ada.Strings.Unbounded.Null_Unbounded_String);
			begin
				Save(Id, User_Info);
				Result := True;
			end;
		end if;
	exception
		when Ada.IO_Exceptions.Name_Error => Result := False;
	end New_User;
	
	procedure Update(Id : String;
		Remote_Addr, Remote_Host : in String;
		Time : in Ada.Calendar.Time;
		User_Info : in out Users.User_Info) is
	begin
		User_Info.Last_Remote_Addr := +Remote_Addr;
		User_Info.Last_Remote_Host := +Remote_Host;
		User_Info.Last_Time := Time;
		Save(Id, User_Info);
	end Update;
	
	function Exists(Id : String) return Boolean is
	begin
		return Ada.Directories.Exists(Ada.Directories.Compose(Configurations.Users_Directory, Id));
	end Exists;
	
	type User_Log_Item is record
		Id : Ada.Strings.Unbounded.Unbounded_String;
		Remote_Addr : Ada.Strings.Unbounded.Unbounded_String;
		Remote_Host : Ada.Strings.Unbounded.Unbounded_String;
	end record;
	
	function "<" (Left, Right : User_Log_Item) return Boolean;
	function "<" (Left, Right : User_Log_Item) return Boolean is
		use type Ada.Strings.Unbounded.Unbounded_String;
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
	
	use type Ada.Calendar.Time;
	package Users_Log is new Ada.Containers.Indefinite_Ordered_Maps(User_Log_Item, Ada.Calendar.Time);

	use Users_Log;
	Log : Users_Log.Map;
	Log_Loaded : Boolean := False;

	procedure Load is
	begin
		if not Log_Loaded then
			declare
				File : Ada.Streams.Stream_IO.File_Type;
			begin
				Ada.Streams.Stream_IO.Open(File, Ada.Streams.Stream_IO.In_File, Configurations.Users_Log_File_Name);
				begin
					Users_Log.Map'Read(Ada.Streams.Stream_IO.Stream(File), Log);
					Ada.Streams.Stream_IO.Close(File);
					Log_Loaded := True;
				exception
					when others =>
						Ada.Streams.Stream_IO.Close(File);
						raise;
				end;
			exception
				when Ada.IO_Exceptions.Name_Error => null;
			end;
		end if;
	end Load;

	procedure Add_To_Users_Log(User_Id, Remote_Addr, Remote_Host : String; Time : Ada.Calendar.Time) is
		Item : User_Log_Item := (+User_Id, +Remote_Addr, +Remote_Host);
	begin
		Load;
		Include(Log, Item, Time);
		declare
			File : Ada.Streams.Stream_IO.File_Type;
		begin
			Ada.Streams.Stream_IO.Create(File, Ada.Streams.Stream_IO.Out_File, Configurations.Users_Log_File_Name);
			begin
				Users_Log.Map'Write(Ada.Streams.Stream_IO.Stream(File), Log);
				Ada.Streams.Stream_IO.Close(File);
			exception
				when others =>
					Ada.Streams.Stream_IO.Close(File);
					raise;
			end;
		end;
	end Add_To_Users_Log;

	function Muramura_Count(Time : Ada.Calendar.Time) return Natural is
		Muramura_Set : Users_Log.Map;
		procedure Process(Position : Cursor) is
		begin
			if Time - Element(Position) <= Muramura_Duration then
				declare
					Item : User_Log_Item := (Key(Position).Id, 
						Ada.Strings.Unbounded.Null_Unbounded_String,
						Ada.Strings.Unbounded.Null_Unbounded_String);
				begin
					Include(Muramura_Set, Item, Time);
				end;
			end if;
		end Process;
	begin
		Load;
		Iterate(Log, Process'Access);
		return Natural(Length(Muramura_Set));
	end Muramura_Count;

end Tabula.Users.Managing;

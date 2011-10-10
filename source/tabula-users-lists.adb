-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Streams.Stream_IO;
with Tabula.Users.Load;
with Tabula.Users.Save;
package body Tabula.Users.Lists is
	use type Ada.Strings.Unbounded.Unbounded_String;
	
	procedure Load_Users_Log (List : in out User_List) is
	begin
		if not List.Log_Read then
			begin
				declare
					File : Ada.Streams.Stream_IO.File_Type :=
						Ada.Streams.Stream_IO.Open (Ada.Streams.Stream_IO.In_File, List.Log_File_Name.all);
				begin
					Users_Log.Map'Read (Ada.Streams.Stream_IO.Stream (File), List.Log);
					Ada.Streams.Stream_IO.Close (File);
				end;
			exception
				when Ada.IO_Exceptions.Name_Error => null;
			end;
			List.Log_Read := True;
		end if;
	end Load_Users_Log;
	
	procedure Add_To_Users_Log (
		List : in out User_List;
		Id : in String;
		Remote_Addr : in String;
		Remote_Host : in String;
		Now : in Ada.Calendar.Time)
	is
		Item : User_Log_Item := (+Id, +Remote_Addr, +Remote_Host);
	begin
		Load_Users_Log (List);
		Users_Log.Include (List.Log, Item, Now);
		declare
			File : Ada.Streams.Stream_IO.File_Type :=
				Ada.Streams.Stream_IO.Create (Ada.Streams.Stream_IO.Out_File, List.Log_File_Name.all);
		begin
			Users_Log.Map'Write (Ada.Streams.Stream_IO.Stream (File), List.Log);
			Ada.Streams.Stream_IO.Close (File);
		end;
	end Add_To_Users_Log;
	
	-- implementation
	
	function Create (
		Directory : not null Static_String_Access;
		Log_File_Name : not null Static_String_Access)
		return User_List is
	begin
		return (
			Directory => Directory,
			Log_File_Name => Log_File_Name,
			Log_Read => False,
			Log => Users_Log.Empty_Map);
	end Create;
	
	function Exists (List : User_List; Id : String) return Boolean is
	begin
		return Ada.Directories.Exists (Ada.Directories.Compose (List.Directory.all, Id));
	end Exists;
	
	procedure Query (
		List : in out User_List;
		Id : in String;
		Password : in String;
		Remote_Addr : in String;
		Remote_Host : in String;
		Now : in Ada.Calendar.Time;
		Info : out User_Info;
		State : out User_State) is
	begin
		if Id = "" then
			State := Log_Off;
		elsif not Exists (List, Id) then
			State := Unknown;
		else
			Load (Ada.Directories.Compose (List.Directory.all, Id), Info);
			if Info.Password /= Digest (Password)
				or else Info.Renamed /= Ada.Strings.Unbounded.Null_Unbounded_String
			then
				State := Invalid;
			else
				State := Valid;
				if Id /= Administrator then
					if not Info.No_Log then
						Add_To_Users_Log (
							List,
							Id => Id,
							Remote_Addr => Remote_Addr,
							Remote_Host => Remote_Host,
							Now => Now);
					else
						Add_To_Users_Log (
							List,
							Id => Id,
							Remote_Addr => "",
							Remote_Host => "",
							Now => Now); -- log only time
					end if;
				end if;
			end if;
		end if;
	end Query;
	
	procedure New_User (
		List : in out User_List;
		Id : in String;
		Password : in String;
		Remote_Addr : in String;
		Remote_Host : in String;
		Now : in Ada.Calendar.Time;
		Result : out Boolean) is
	begin
		if Exists (List, Id) then
			Result := False;
		else
			declare
				Info : User_Info := (
					Password => Digest (Password),
					Creation_Remote_Addr => +Remote_Addr,
					Creation_Remote_Host => +Remote_Host,
					Creation_Time => Now,
					Last_Remote_Addr => +Remote_Addr,
					Last_Remote_Host => +Remote_Host,
					Last_Time => Now,
					Ignore_Request => False,
					Disallow_New_Village => False,
					No_Log => False,
					Renamed => Ada.Strings.Unbounded.Null_Unbounded_String);
			begin
				Save (Ada.Directories.Compose (List.Directory.all, Id), Info);
				Result := True;
			end;
		end if;
	exception
		when Ada.IO_Exceptions.Name_Error => Result := False;
	end New_User;
	
	procedure Update (
		List : in out User_List;
		Id : in String;
		Remote_Addr : in String;
		Remote_Host : in String;
		Now : in Ada.Calendar.Time;
		Info : in out User_Info) is
	begin
		Info.Last_Remote_Addr := +Remote_Addr;
		Info.Last_Remote_Host := +Remote_Host;
		Info.Last_Time := Now;
		Save (Ada.Directories.Compose (List.Directory.all, Id), Info);
	end Update;
	
	function All_Users (List : User_List) return User_Info_Maps.Map is
		Search : Ada.Directories.Search_Type;
		File : Ada.Directories.Directory_Entry_Type;
	begin
		return Result : User_Info_Maps.Map do
			Ada.Directories.Start_Search (
				Search,
				List.Directory.all,
				"*",
				Filter => (Ada.Directories.Ordinary_File => True, others => False));
			while Ada.Directories.More_Entries(Search) loop
				Ada.Directories.Get_Next_Entry(Search, File);
				declare
					Id : String := Ada.Directories.Simple_Name (File);
				begin
					if Id (Id'First) /= '.' then -- excluding dot file
						declare
							Info : User_Info;
						begin
							Load (Ada.Directories.Compose (List.Directory.all, Id), Info);
							User_Info_Maps.Include (Result, Id, Info);
						end;
					end if;
				end;
			end loop;
			Ada.Directories.End_Search(Search);
		end return;
	end All_Users;
	
	procedure Muramura_Count (
		List : in out User_List;
		Now : Ada.Calendar.Time;
		Muramura_Duration : Duration;
		Result : out Natural)
	is
		Muramura_Set : Users_Log.Map;
		procedure Process (Position : Users_Log.Cursor) is
		begin
			if Now - Users_Log.Element(Position) <= Muramura_Duration then
				declare
					Item : User_Log_Item := (Users_Log.Key(Position).Id,
						Ada.Strings.Unbounded.Null_Unbounded_String,
						Ada.Strings.Unbounded.Null_Unbounded_String);
				begin
					Users_Log.Include (Muramura_Set, Item, Now);
				end;
			end if;
		end Process;
	begin
		Load_Users_Log (List);
		Users_Log.Iterate (List.Log, Process'Access);
		Result := Muramura_Set.Length;
	end Muramura_Count;
	
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
	
	procedure Iterate_Log (
		List : in out User_List;
		Process : not null access procedure (
			Id : in String;
			Remote_Addr : in String;
			Remote_Host : in String;
			Time : in Ada.Calendar.Time))
	is
		procedure Thunk (Position : in Users_Log.Cursor) is
			Key : User_Log_Item renames Users_Log.Key (Position);
		begin
			Process (
				Id => Key.Id.Constant_Reference.Element.all,
				Remote_Addr => Key.Remote_Addr.Constant_Reference.Element.all,
				Remote_Host => Key.Remote_Host.Constant_Reference.Element.all,
				Time => List.Log.Constant_Reference (Position).Element.all);
		end Thunk;
	begin
		Load_Users_Log (List);
		Users_Log.Iterate (List.Log, Thunk'Access);
	end Iterate_Log;
	
end Tabula.Users.Lists;

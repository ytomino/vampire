-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Containers.Indefinite_Ordered_Maps;
private with Ada.Containers.Ordered_Maps;
private with Ada.Strings.Unbounded;
package Tabula.Users.Lists is
	
	type User_List (<>) is limited private;
	
	function Create (
		Directory : not null Static_String_Access;
		Log_File_Name : not null Static_String_Access)
		return User_List;
	
	-- 問い合わせ
	
	function Exists (List : User_List; Id : String) return Boolean;
	
	type User_State is (Unknown, Invalid, Log_Off, Valid);
	
	procedure Query (
		List : in out User_List;
		Id : in String;
		Password : in String;
		Remote_Addr : in String;
		Remote_Host : in String;
		Now : in Ada.Calendar.Time;
		Info : out User_Info;
		State : out User_State);
	
	procedure New_User (
		List : in out User_List;
		Id : in String;
		Password : in String;
		Remote_Addr : in String;
		Remote_Host : in String;
		Now : in Ada.Calendar.Time;
		Result : out Boolean);
	
	procedure Update (
		List : in out User_List;
		Id : in String;
		Remote_Addr : in String;
		Remote_Host : in String;
		Now : in Ada.Calendar.Time;
		Info : in out User_Info);
	
	-- 全ユーザーのリスト
	
	package User_Info_Maps is
		new Ada.Containers.Indefinite_Ordered_Maps (String, User_Info);
	
	function All_Users (List : User_List) return User_Info_Maps.Map;
	
	-- ムラムラスカウター(仮)
	
	procedure Muramura_Count (
		List : in out User_List;
		Now : Ada.Calendar.Time;
		Muramura_Duration : Duration;
		Result : out Natural);
	
private
	use type Ada.Calendar.Time;
	
	type User_Log_Item is record
		Id : aliased Ada.Strings.Unbounded.Unbounded_String;
		Remote_Addr : aliased Ada.Strings.Unbounded.Unbounded_String;
		Remote_Host : aliased Ada.Strings.Unbounded.Unbounded_String;
	end record;
	
	function "<" (Left, Right : User_Log_Item) return Boolean;
	
	package Users_Log is
		new Ada.Containers.Ordered_Maps (User_Log_Item, Ada.Calendar.Time);
	
	type User_List is limited record
		Directory : not null Static_String_Access;
		Log_File_Name : not null Static_String_Access;
		Log_Read : Boolean;
		Log : aliased Users_Log.Map;
	end record;
	
	-- log
	
	procedure Iterate_Log (
		List : in out User_List;
		Process : not null access procedure (
			Id : in String;
			Remote_Addr : in String;
			Remote_Host : in String;
			Time : in Ada.Calendar.Time));
	
end Tabula.Users.Lists;

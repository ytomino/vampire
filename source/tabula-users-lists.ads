-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Containers.Indefinite_Ordered_Maps;
package Tabula.Users.Lists is
	
	-- データベース
	
	type Users_List (<>) is limited private;
	
	function Create (
		Directory : not null Static_String_Access;
		Log_File_Name : not null Static_String_Access)
		return Users_List;
	
	-- 問い合わせ
	
	function Exists (List : Users_List; Id : String) return Boolean;
	
	type User_State is (Unknown, Invalid, Log_Off, Valid);
	
	procedure Query (
		List : in Users_List;
		Id : in String;
		Password : in String; 
		Remote_Addr : in String;
		Remote_Host : in String;
		Now : in Ada.Calendar.Time;
		Info : out User_Info;
		State : out User_State);
	
	procedure New_User (
		List : in out Users_List;
		Id : in String;
		Password : in String;
		Remote_Addr : in String;
		Remote_Host : in String;
		Now : in Ada.Calendar.Time;
		Result : out Boolean);
	
	procedure Update (
		List : in out Users_List;
		Id : in String;
		Remote_Addr : in String;
		Remote_Host : in String;
		Now : in Ada.Calendar.Time;
		Info : in out User_Info);
	
	-- 全ユーザーのリスト
	
	package User_Info_Maps is new Ada.Containers.Indefinite_Ordered_Maps (String, User_Info);
	
	function All_Users (List : Users_List) return User_Info_Maps.Map;
	
	-- ムラムラスカウター(仮)
	
	function Muramura_Count (List : Users_List; Now : Ada.Calendar.Time) return Natural;
	
private
	
	type Users_List is limited record
		Directory : not null Static_String_Access;
		Log_File_Name : not null Static_String_Access;
		Log_Read_Count : Natural := 0; -- for performance check
	end record;
	
end Tabula.Users.Lists;

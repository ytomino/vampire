-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Containers.Indefinite_Ordered_Maps;
package Tabula.Users.Lists is
	
	type Check_Result is (Log_Off, Unknown, Invalid, Valid);
	
	procedure Check (
		Id : in String;
		Password : in String; 
		Remote_Addr : in String;
		Remote_Host : in String;
		Now : in Ada.Calendar.Time;
		Result : out Check_Result; 
		User_Info : out Users.User_Info);
	
	procedure New_User (
		Id : in String;
		Password : in String;
		Remote_Addr : in String;
		Remote_Host : in String;
		Now : in Ada.Calendar.Time;
		Result : out Boolean);
	
	procedure Update (
		Id : String;
		Remote_Addr : in String;
		Remote_Host : in String;
		Time : in Ada.Calendar.Time;
		User_Info : in out Users.User_Info);
	
	function Exists (Id : String) return Boolean;
	
	procedure Add_To_Users_Log (
		User_Id : in String; 
		Remote_Addr : in String; 
		Remote_Host : in String;
		Time : in Ada.Calendar.Time);
	
	function Muramura_Count (Time : Ada.Calendar.Time) return Natural;
	
	package User_Info_Maps is new Ada.Containers.Indefinite_Ordered_Maps (String, User_Info);
	
	function User_List return User_Info_Maps.Map;
	
end Tabula.Users.Lists;

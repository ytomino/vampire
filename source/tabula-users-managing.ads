-- The Village of Vampire by YT, このソースコードはNYSLです
package Tabula.Users.Managing is
	
	type Check_Result is (Log_Off, Unknown, Invalid, Valid);
	
	procedure Check(Id, Password : in String; 
		Remote_Addr, Remote_Host : in String;
		Now : in Ada.Calendar.Time;
		Result : out Check_Result; 
		User_Info : out Users.User_Info);
	
	procedure New_User(Id, Password : in String;
		Remote_Addr, Remote_Host : in String;
		Now : in Ada.Calendar.Time;
		Result : out Boolean);
	
	procedure Update(Id : String;
		Remote_Addr, Remote_Host : in String;
		Time : in Ada.Calendar.Time;
		User_Info : in out Users.User_Info);
	
	function Exists(Id : String) return Boolean;
	
	procedure Add_To_Users_Log(User_Id, Remote_Addr, Remote_Host : in String; Time : in Ada.Calendar.Time);
	function Muramura_Count(Time : Ada.Calendar.Time) return Natural;
	
end Tabula.Users.Managing;

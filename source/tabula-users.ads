-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Calendar;
with Ada.Strings.Unbounded;
with Ase.MD5;
with Tabula.Calendar;
package Tabula.Users is
	
	Administrator : constant String := "administrator";
	
	function Valid_Id_String(Id : String) return Boolean;
	function Digest(Password : String) return Ase.MD5.Message_Digest;
	
	type User_Info is record
		-- MD5ハッシュ済みパスワード
		Password : Ada.Strings.Unbounded.Unbounded_String;
		-- ユーザー作成時の情報
		Remote_Addr : Ada.Strings.Unbounded.Unbounded_String;
		Remote_Host : Ada.Strings.Unbounded.Unbounded_String;
		Creation_Time : Ada.Calendar.Time := Tabula.Calendar.Null_Time;
		-- 最終アクセス時の情報
		Last_Remote_Addr : Ada.Strings.Unbounded.Unbounded_String;
		Last_Remote_Host : Ada.Strings.Unbounded.Unbounded_String;
		Last_Time : Ada.Calendar.Time := Tabula.Calendar.Null_Time;
		-- ペナルティなど
		Ignore_Request : Boolean := False;
		Disallow_New_Village : Boolean := False;
		No_Log : Boolean := False;
		-- IDを変更した場合
		Renamed : Ada.Strings.Unbounded.Unbounded_String;
	end record;
	
end Tabula.Users;

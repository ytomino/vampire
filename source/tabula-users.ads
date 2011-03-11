-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Calendar;
with Ada.Strings.Unbounded;
with Tabula.Calendar;
private with Ada.Streams;
package Tabula.Users is
	
	-- Id
	
	Administrator : constant String := "administrator";
	
	function Valid_Id_String (Id : String) return Boolean;
	
	-- Password
	
	type Password_Digest is private;
	
	Null_Password_Digest : constant Password_Digest;
	
	function Digest (Password : String) return Password_Digest;
	
	subtype Password_Digest_Image is String (1 .. 32);
	
	function Image (Digest : Password_Digest) return Password_Digest_Image;
	function Value (Image : Password_Digest_Image) return Password_Digest;
	
	-- User Info
	
	type User_Info is record
		-- MD5ハッシュ済みパスワード
		Password : Password_Digest;
		-- ユーザー作成時の情報
		Creation_Remote_Addr : Ada.Strings.Unbounded.Unbounded_String;
		Creation_Remote_Host : Ada.Strings.Unbounded.Unbounded_String;
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
	
private
	
	type Password_Digest is new Ada.Streams.Stream_Element_Array (0 .. 15);
	
	Null_Password_Digest : constant Password_Digest := (others => 0);
	
end Tabula.Users;

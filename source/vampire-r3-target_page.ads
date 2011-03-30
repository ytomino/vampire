-- The Village of Vampire by YT, このソースコードはNYSLです
-- 医者と探偵の確認用ページ
with Vampire.Villages;
procedure Vampire.R3.Target_Page (
	Output : not null access Ada.Streams.Root_Stream_Type'Class;
	Form : in Forms.Root_Form_Type'Class;
	Template : in String;
	Village_Id : in Tabula.Villages.Village_Id;
	Village : in Villages.Village_Type;
	Player : in Tabula.Villages.Person_Index;
	Target : in Tabula.Villages.Person_Index;
	User_Id : in String;
	User_Password : in String);

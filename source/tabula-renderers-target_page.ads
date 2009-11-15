-- The Village of Vampire by YT, このソースコードはNYSLです
-- 医者と探偵の確認用ページ
procedure Tabula.Renderers.Target_Page (
	Object : in Renderer'Class;
	Output : not null access Ada.Streams.Root_Stream_Type'Class;
	Village_Id : in Villages.Village_Id;
	Village : in Vampires.Villages.Village_Type;
	Player : in Natural;
	Target : in Natural;
	User_Id : in String;
	User_Password : in String);

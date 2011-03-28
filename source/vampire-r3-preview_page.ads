-- The Village of Vampire by YT, このソースコードはNYSLです
procedure Vampire.Renderers.Preview_Page (
	Object : in Renderer'Class;
	Output : not null access Ada.Streams.Root_Stream_Type'Class;
	Village_Id : in Tabula.Villages.Village_Id;
	Village : in Vampire.Villages.Village_Type; 
	Message : in Vampire.Villages.Message;
	User_Id : in String;
	User_Password : in String);

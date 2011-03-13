-- The Village of Vampire by YT, このソースコードはNYSLです
procedure Tabula.Renderers.Preview_Page (
	Object : in Renderer'Class;
	Output : not null access Ada.Streams.Root_Stream_Type'Class;
	Village_Id : in Villages.Village_Id;
	Village : in Vampires.Villages.Village_Type; 
	Message : in Vampires.Villages.Message;
	User_Id : in String;
	User_Password : in String);

procedure Tabula.Renderers.Message_Page(
	Object : in Renderer'Class;
	Output : not null access Ada.Streams.Root_Stream_Type'Class;
	Village_Id : in Villages.Lists.Village_Id := Villages.Lists.Invalid_Village_Id;
	Village : access Vampires.Villages.Village_Type := null;
	Message : in String;
	User_Id : in String;
	User_Password : in String);

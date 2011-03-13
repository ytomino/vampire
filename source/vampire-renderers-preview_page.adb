-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Strings.Unbounded;
procedure Tabula.Renderers.Preview_Page (
	Object : in Renderer'Class;
	Output : not null access Ada.Streams.Root_Stream_Type'Class;
	Village_Id : in Villages.Village_Id;
	Village : in Vampires.Villages.Village_Type; 
	Message : in Vampires.Villages.Message;
	User_Id : in String;
	User_Password : in String)
is
	function "+" (S : Ada.Strings.Unbounded.Unbounded_String) return String renames Ada.Strings.Unbounded.To_String;
	procedure Handle(Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Tag : in String; Template : in Web.Producers.Template) is
	begin
		if Tag = "kind" then
			Write(Output, '"');
			Web.Write_In_Attribute (Output, Object.HTML_Version, Vampires.Villages.Message_Kind'Image (Message.Kind));
			Write(Output, '"');
		elsif Tag = "value" then
			Write(Output, '"');
			Web.Write_In_Attribute (Output, Object.HTML_Version, +Message.Text);
			Write(Output, '"');
		elsif Tag = "longer" then
			if Ada.Strings.Unbounded.Length(Message.Text) > Max_Length_Of_Message then
				Web.Producers.Produce(Output, Template);
			end if;
		elsif Tag = "ok" then
			if Ada.Strings.Unbounded.Length(Message.Text) <= Max_Length_Of_Message then
				Web.Producers.Produce(Output, Template, Handler => Handle'Access);
			end if;
		else
			Handle_Messages(Output, Tag, Template, Object,
				Village_Id, Village, Village.Today, Message, Message.Time, User_Id => User_Id, User_Password => User_Password);
		end if;
	end Handle;
begin
	Produce (Object, Output, Object.Configuration.Template_Preview_File_Name.all, Handle'Access);
end Tabula.Renderers.Preview_Page;

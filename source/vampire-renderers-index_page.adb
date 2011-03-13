-- The Village of Vampire by YT, このソースコードはNYSLです
procedure Vampire.Renderers.Index_Page (
	Object : in Renderer'Class;
	Output : not null access Ada.Streams.Root_Stream_Type'Class;
	Summaries : in out Tabula.Villages.Lists.Summary_Maps.Map;
	Muramura : in Natural;
	User_Id: in String;
	User_Password : in String)
is
	procedure Handle(Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Tag : in String; Template : in Web.Producers.Template) is
	begin
		if Tag = "userpanel" then
			User_Panel (Object, Output, Template, User_Id, User_Password, True);
		elsif Tag = "muramura" then
			for I in 1 .. Muramura loop
				Web.Producers.Produce(Output, Template);
			end loop;
		else
			Handle_List (Output, Tag, Template, Object, Summaries, 10,
				User_Id => User_Id, User_Password => User_Password);
		end if;
	end Handle;
begin
	Produce (Object, Output, Object.Configuration.Template_Index_File_Name.all, Handle'Access);
end Vampire.Renderers.Index_Page;

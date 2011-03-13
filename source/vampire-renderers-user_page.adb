-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Strings.Unbounded;
procedure Vampire.Renderers.User_Page (
	Object : in Renderer'Class;
	Output : not null access Ada.Streams.Root_Stream_Type'Class;
	Summaries : in Tabula.Villages.Lists.Summary_Maps.Map;
	User_Id : in String;
	User_Password : in String;
	User_Info : in Users.User_Info)
is
	use type Ada.Strings.Unbounded.Unbounded_String;
	use Tabula.Villages;
	use Tabula.Villages.Lists.Summary_Maps;
	use Tabula.Villages.Lists.User_Lists;
	use Villages;
	function "+" (S : Ada.Strings.Unbounded.Unbounded_String) return String renames Ada.Strings.Unbounded.To_String;
	Joined : Ada.Strings.Unbounded.Unbounded_String;
	Created : Ada.Strings.Unbounded.Unbounded_String;
	procedure Handle(Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Tag : in String; Template : in Web.Producers.Template) is
	begin
		if Tag = "userpanel" then
			User_Panel (Object, Output, Template, User_Id, User_Password, False);
		elsif Tag = "back" then
			Write(Output, "<a href=");
			Link (Object, Output, User_Id => User_Id, User_Password => User_Password);
			Write(Output, '>');
			Web.Producers.Produce(Output, Template);
			Write(Output, "</a>");
		elsif Tag = "joined" then
			if Joined /= Ada.Strings.Unbounded.Null_Unbounded_String then
				Web.Producers.Produce(Output, Template, Handler => Handle'Access);
			end if;
		elsif Tag = "nojoined" then
			if Joined = Ada.Strings.Unbounded.Null_Unbounded_String then
				Web.Producers.Produce(Output, Template);
			end if;
		elsif Tag = "created" then
			if Created /= Ada.Strings.Unbounded.Null_Unbounded_String then
				Web.Producers.Produce(Output, Template, Handler => Handle'Access);
			end if;
		elsif Tag = "creatable" then
			if Joined = Ada.Strings.Unbounded.Null_Unbounded_String
				and then Created = Ada.Strings.Unbounded.Null_Unbounded_String
			then
				Web.Producers.Produce(Output, Template, Handler => Handle'Access);
			end if;
		elsif Tag = "activevillage" then
			Web.Write_In_HTML (Output, Object.HTML_Version, +Joined);
		elsif Tag = "createdvillage" then
			Web.Write_In_HTML (Output, Object.HTML_Version, +Created);
		else
			Handle_Users(Output, Tag, Template, Object,
				User_Id => User_Id, User_Password => User_Password);
		end if;
	end Handle;
	I : Lists.Summary_Maps.Cursor := Summaries.First;
begin
	while Has_Element (I) loop
		declare
			V : Lists.Village_Summary
				renames Summaries.Constant_Reference (I).Element.all;
		begin
			if V.State < Epilogue then
				declare
					J : Lists.User_Lists.Cursor := V.People.First;
				begin
					while Has_Element (J) loop
						if Element(J) = User_Id then
							if Joined /= Ada.Strings.Unbounded.Null_Unbounded_String then
								Ada.Strings.Unbounded.Append(Joined, "、");
							end if;
							Ada.Strings.Unbounded.Append(Joined, V.Name);
						end if;
						Next(J);
					end loop;
				end;
				if V.By = User_Id then
					Created := V.Name;
				end if;
			end if;
		end;
		Next (I);
	end loop;
	Produce (Object, Output, Object.Configuration.Template_User_File_Name.all, Handle'Access);
end Vampire.Renderers.User_Page;

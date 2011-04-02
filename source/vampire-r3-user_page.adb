-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Strings.Unbounded;
procedure Vampire.R3.User_Page (
	Output : not null access Ada.Streams.Root_Stream_Type'Class;
	Form : in Forms.Root_Form_Type'Class;
	Template : in String;
	Summaries : in Tabula.Villages.Lists.Summary_Maps.Map;
	User_Id : in String;
	User_Password : in String;
	User_Info : in Users.User_Info)
is
	use Tabula.Villages.Lists.Summary_Maps;
	use Tabula.Villages.Lists.User_Lists;
	use type Ada.Strings.Unbounded.Unbounded_String;
	use type Tabula.Villages.Village_State;
	-- ユーザーの参加状況
	Joined : aliased Ada.Strings.Unbounded.Unbounded_String;
	Created : aliased Ada.Strings.Unbounded.Unbounded_String;
	procedure Handle(
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Tag : in String;
		Template : in Web.Producers.Template) is
	begin
		if Tag = "action_page" then
			Forms.Write_Attribute_Name (Output, "action");
			Forms.Write_Link (
				Output,
				Form,
				Current_Directory => ".",
				Resource => Forms.Self,
				Parameters => Form.Parameters_To_User_Page (
					User_Id => User_Id,
					User_Password => User_Password));
		elsif Tag = "userpanel" then
			Handle_User_Panel (
				Output,
				Template,
				Form,
				User_Id => User_Id,
				User_Password => User_Password);
		elsif Tag = "id" then
			Forms.Write_In_HTML (Output, Form, User_Id);
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
			Forms.Write_In_HTML (Output, Form, Joined.Constant_Reference.Element.all);
		elsif Tag = "createdvillage" then
			Forms.Write_In_HTML (Output, Form, Created.Constant_Reference.Element.all);
		elsif Tag = "href_index" then
			Forms.Write_Attribute_Name (Output, "href");
			Forms.Write_Link (
				Output,
				Form,
				Current_Directory => ".",
				Resource => Forms.Self,
				Parameters => Form.Parameters_To_Index_Page (
					User_Id => User_Id,
					User_Password => User_Password));
		else
			raise Program_Error with "Invalid template """ & Tag & """";
		end if;
	end Handle;
	I : Tabula.Villages.Lists.Summary_Maps.Cursor := Summaries.First;
begin
	while Has_Element (I) loop
		declare
			V : Tabula.Villages.Lists.Village_Summary
				renames Summaries.Constant_Reference (I).Element.all;
		begin
			if V.State < Tabula.Villages.Epilogue then
				declare
					J : Tabula.Villages.Lists.User_Lists.Cursor := V.People.First;
				begin
					while Has_Element (J) loop
						if Element (J) = User_Id then
							if Joined /= Ada.Strings.Unbounded.Null_Unbounded_String then
								Ada.Strings.Unbounded.Append(Joined, "、");
							end if;
							Ada.Strings.Unbounded.Append(Joined, V.Name);
						end if;
						Next (J);
					end loop;
				end;
				if V.By = User_Id then
					Created := V.Name;
				end if;
			end if;
		end;
		Next (I);
	end loop;
	Web.Producers.Produce (Output, Read (Template), Handler => Handle'Access);
end Vampire.R3.User_Page;

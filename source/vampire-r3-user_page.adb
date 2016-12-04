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
	use type Ada.Strings.Unbounded.Unbounded_String;
	use type Tabula.Villages.Village_State;
	-- ユーザーの参加状況
	Joined : aliased Ada.Strings.Unbounded.Unbounded_String;
	Created : aliased Ada.Strings.Unbounded.Unbounded_String;
begin
	for I in Summaries.Iterate loop
		declare
			V : Tabula.Villages.Lists.Village_Summary
				renames Summaries.Constant_Reference (I);
		begin
			if V.State < Tabula.Villages.Epilogue then
				for J in V.People.Iterate loop
					if V.People.Constant_Reference (J) = User_Id then
						if not Joined.Is_Null then
							Ada.Strings.Unbounded.Append(Joined, "、");
						end if;
						Ada.Strings.Unbounded.Append(Joined, V.Name);
					end if;
				end loop;
				if V.By = User_Id then
					Created := V.Name;
				end if;
			end if;
		end;
	end loop;
	declare
		procedure Handle (
			Output : not null access Ada.Streams.Root_Stream_Type'Class;
			Tag : in String;
			Contents : in Web.Producers.Template) is
		begin
			if Tag = "action_page" then
				Forms.Write_Attribute_Name (Output, "action");
				Forms.Write_Link (
					Output,
					Form,
					Current_Directory => ".",
					Resource => Forms.Self,
					Parameters =>
						Form.Parameters_To_User_Page (
							User_Id => User_Id,
							User_Password => User_Password));
			elsif Tag = "userpanel" then
				Handle_User_Panel (
					Output,
					Contents,
					Form,
					User_Id => User_Id,
					User_Password => User_Password);
			elsif Tag = "id" then
				Forms.Write_In_HTML (Output, Form, User_Id);
			elsif Tag = "joined" then
				if not Joined.Is_Null then
					declare
						procedure Handle (
							Output : not null access Ada.Streams.Root_Stream_Type'Class;
							Tag : in String;
							Contents : in Web.Producers.Template) is
						begin
							if Tag = "activevillage" then
								Forms.Write_In_HTML (Output, Form, Joined.Constant_Reference);
							else
								Raise_Unknown_Tag (Tag);
							end if;
						end Handle;
					begin
						Web.Producers.Produce(Output, Contents, Handler => Handle'Access);
					end;
				end if;
			elsif Tag = "nojoined" then
				if Joined.Is_Null then
					Web.Producers.Produce(Output, Contents);
				end if;
			elsif Tag = "created" then
				if not Created.Is_Null then
					declare
						procedure Handle (
							Output : not null access Ada.Streams.Root_Stream_Type'Class;
							Tag : in String;
							Contents : in Web.Producers.Template) is
						begin
							if Tag = "createdvillage" then
								Forms.Write_In_HTML (Output, Form, Created.Constant_Reference);
							else
								Raise_Unknown_Tag (Tag);
							end if;
						end Handle;
					begin
						Web.Producers.Produce(Output, Contents, Handler => Handle'Access);
					end;
				end if;
			elsif Tag = "creatable" then
				if Joined.Is_Null and then Created.Is_Null then
					Web.Producers.Produce(Output, Contents, Handler => Handle'Access); -- rec
				end if;
			elsif Tag = "href_index" then
				Forms.Write_Attribute_Name (Output, "href");
				Forms.Write_Link (
					Output,
					Form,
					Current_Directory => ".",
					Resource => Forms.Self,
					Parameters =>
						Form.Parameters_To_Index_Page (
							User_Id => User_Id,
							User_Password => User_Password));
			else
				Raise_Unknown_Tag (Tag);
			end if;
		end Handle;
	begin
		Web.Producers.Produce (Output, Read (Template), Handler => Handle'Access);
	end;
end Vampire.R3.User_Page;

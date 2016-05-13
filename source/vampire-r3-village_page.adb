-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Calendar.Formatting;
with Ada.Hierarchical_File_Names;
with Ada.Numerics.Distributions;
with Ada.Numerics.MT19937;
with Ada.Strings.Unbounded;
with Tabula.Calendar;
with Tabula.Casts.Load;
with Tabula.Users;
with Vampire.Villages.Teaming;
with Vampire.Villages.Text;
procedure Vampire.R3.Village_Page (
	Output : not null access Ada.Streams.Root_Stream_Type'Class;
	Form : in Forms.Root_Form_Type'Class;
	Template : in Web.Producers.Template;
	Current_Directory : in String;
	HTML_Directory : in String;
	Image_Directory : in String;
	Style_Sheet : in String;
	Background : in String;
	Relative_Role_Images : in Villages.Role_Images;
	Cast_File_Name : in String;
	Log : in Boolean;
	Village_Id : in Tabula.Villages.Village_Id;
	Village : in Villages.Village_Type;
	Day : in Natural;
	Showing_Range : in Tabula.Villages.Message_Range_Type := (
		First => Tabula.Villages.Message_Index'First,
		Last => Tabula.Villages.Message_Index'Last - 2);
	Editing : Villages.Message_Kind := Villages.Speech;
	Editing_Text : String := "";
	User_Id : in String;
	User_Password : in String)
is
	use Tabula.Villages;
	use Villages;
	use type Ada.Calendar.Time;
	use type Ada.Strings.Unbounded.Unbounded_String;
	use type Forms.Template_Set_Type;
	
	Line_Break : constant Character := ASCII.LF;
	
	procedure Rule_Panel (
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Template : in Web.Producers.Template;
		Village_Id : in Tabula.Villages.Village_Id;
		Village : in Vampire.Villages.Village_Type; 
		Player : in Boolean;
		User_Id : in String;
		User_Password : in String)
	is
		Visible : constant Boolean :=
			Player
			or else Village.Term = Short
			or else Village.Option_Changed
			or else Is_Obsolete_Teaming (Village.Obsolete_Teaming);
		Changable : constant Boolean :=
			Player
			and then Village.Today = 0
			and then Village.No_Commit;
		procedure Handle (
			Output : not null access Ada.Streams.Root_Stream_Type'Class;
			Tag : in String;
			Template : in Web.Producers.Template) is
		begin
			if Tag = "items" then
				declare
					procedure Process (Item : in Root_Option_Item'Class) is
						procedure Handle_Item (
							Output : not null access Ada.Streams.Root_Stream_Type'Class;
							Tag : in String;
							Template : in Web.Producers.Template) is
						begin
							if Tag = "current" then
								declare
									procedure Process (
										Value : in String;
										Selected : in Boolean;
										Message : in String;
										Unrecommended : in Boolean) is
									begin
										if Selected then
											if Item.Changed and then Village.State /= Closed then
												String'Write (Output, "<em>");
											end if;
											Forms.Write_In_HTML (Output, Form, Message);
											if Village.State /= Closed and then Unrecommended then
												Forms.Write_In_HTML (Output, Form, " (非推奨)");
											end if;
											if Item.Changed and then Village.State /= Closed then
												String'Write (Output, "</em>");
											end if;
										end if;
									end Process;
								begin
									Tabula.Villages.Iterate (Item, Process'Access);
								end;
							elsif Tag = "select" then
								String'Write (Output, "<select ");
								Forms.Write_Attribute_Name (Output, "name");
								Forms.Write_Attribute_Open (Output);
								Forms.Write_In_Attribute (Output, Form, Item.Name);
								Forms.Write_Attribute_Close (Output);
								Character'Write (Output, '>');
								declare
									procedure Process (
										Value : in String;
										Selected : in Boolean;
										Message : in String;
										Unrecommended : in Boolean) is
									begin
										String'Write (Output, "<option ");
										Forms.Write_Attribute_Name (Output, "value");
										Forms.Write_Attribute_Open (Output);
										Forms.Write_In_Attribute (Output, Form, Value);
										Forms.Write_Attribute_Close (Output);
										Character'Write (Output, ' ');
										if Selected then
											Forms.Write_Attribute_Name (Output, "selected");
											Forms.Write_Attribute_Open (Output);
											Forms.Write_In_Attribute (Output, Form, "selected");
											Forms.Write_Attribute_Close (Output);
										end if;
										Character'Write (Output, '>');
										Forms.Write_In_HTML (Output, Form, Message);
										if Unrecommended then
											Forms.Write_In_HTML (Output, Form, " (非推奨)");
										end if;
										if Selected then
											Forms.Write_In_HTML (Output, Form, " *");
										end if;
										String'Write (Output, "</option>");
									end Process;
								begin
									Tabula.Villages.Iterate (Item, Process'Access);
								end;
								String'Write (Output, "</select>");
							else
								raise Program_Error with "Invalid template """ & Tag & """";
							end if;
						end Handle_Item;
					begin
						if Item.Available and then (Changable or else Item.Changed or else Player) then
							Web.Producers.Produce (Output, Template, Handler => Handle_Item'Access);
						end if;
					end Process;
				begin
					Villages.Iterate_Options (Village, Process'Access);
					if Is_Obsolete_Teaming (Village.Obsolete_Teaming) then
						declare
							procedure Handle_Item (
								Output : not null access Ada.Streams.Root_Stream_Type'Class;
								Tag : in String;
								Template : in Web.Producers.Template) is
							begin
								if Tag = "current" or else Tag = "select" then
									Forms.Write_In_HTML (Output, Form, "旧編成「" & Villages.Text.Image (Village.Obsolete_Teaming) & "」です。");
								else
									raise Program_Error with "Invalid template """ & Tag & """";
								end if;
							end Handle_Item;
						begin
							Web.Producers.Produce (Output, Template, Handler => Handle_Item'Access);
						end;
					end if;
				end;
			elsif Tag = "changable" then
				if Changable then
					Web.Producers.Produce (Output, Template, Handler => Handle'Access);
				end if;
			elsif Tag = "static" then
				if not Changable then
					Web.Producers.Produce (Output, Template, Handler => Handle'Access);
				end if;
			elsif Tag = "action_page" then
				Forms.Write_Attribute_Name (Output, "action");
				Forms.Write_Link (
					Output,
					Form,
					Current_Directory => Current_Directory,
					Resource => Forms.Self,
					Parameters => Form.Parameters_To_Village_Page (
						Village_Id => Village_Id,
						User_Id => User_Id,
						User_Password => User_Password));
			else
				raise Program_Error with "Invalid template """ & Tag & """";
			end if;
		end Handle;
	begin
		if Visible then
			Web.Producers.Produce (Output, Template, Handler => Handle'Access);
		end if;
	end Rule_Panel;
	
	procedure Vote_Form (
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Player : in Integer;
		Kind : in Vampire.Villages.Person_Role;
		Special : in Ability_State;
		Current : in Person_Index'Base;
		Current_Special : in Boolean;
		Message : in String;
		Button : in String)
	is
		Keeping_Silver_Bullet : constant Boolean := Kind = Hunter and then Special = Disallowed;
		Including : Boolean;
	begin
		case Form.Template_Set is
			when Forms.For_Full =>
				String'Write (
					Output,
					"<form method=""POST"" class=""inner"">" & Line_Break &
					"<table><tr>" & Line_Break & "<td class=""input"">");
			when Forms.For_Mobile =>
				String'Write (
					Output,
					"<form method=""POST"" ");
				Forms.Write_Attribute_Name (Output, "action");
				Forms.Write_Link (
					Output,
					Form,
					Current_Directory => Current_Directory,
					Resource => Forms.Self,
					Parameters => Form.Parameters_To_Village_Page (
						Village_Id => Village_Id,
						User_Id => User_Id,
						User_Password => User_Password));
				Character'Write (Output, '>');
		end case;
		Forms.Write_In_HTML (Output, Form, Message);
		String'Write (Output, " <select name=""target"">" & Line_Break);
		for Position in Village.People.First_Index .. Village.People.Last_Index loop
			if Position /= Player then
				case Kind is
					when Vampire.Villages.Inhabitant =>
						Including := Village.People.Constant_Reference(Position).Records.Constant_Reference (Village.Target_Day).State /= Vampire.Villages.Died
							and then Village.People.Constant_Reference(Position).Records.Constant_Reference (Village.Target_Day).Candidate;
					when Vampire.Villages.Detective =>
						Including := Village.People.Constant_Reference(Position).Records.Constant_Reference (Village.Target_Day).State = Vampire.Villages.Died;
					when Vampire.Villages.Vampire_Role =>
						Including := Village.People.Constant_Reference(Position).Records.Constant_Reference (Village.Target_Day).State /= Vampire.Villages.Died
							and then Village.People.Constant_Reference(Position).Role not in Vampire.Villages.Vampire_Role;
					when others =>
						Including := Village.People.Constant_Reference(Position).Records.Constant_Reference (Village.Target_Day).State /= Vampire.Villages.Died;
				end case;
				if Including then
					String'Write(Output, "<option ");
					Forms.Write_Attribute_Name (Output, "value");
					Forms.Write_Attribute_Open (Output);
					Forms.Write_In_Attribute (Output, Form, Image (Position));
					Forms.Write_Attribute_Close (Output);
					Character'Write (Output, ' ');
					if Current = Position then
						Forms.Write_Attribute_Name (Output, "selected");
						Forms.Write_Attribute_Open (Output);
						Forms.Write_In_Attribute (Output, Form, "selected");
						Forms.Write_Attribute_Close (Output);
					end if;
					Character'Write (Output, '>');
					Forms.Write_In_HTML (Output, Form, Villages.Text.Name(Village.People.Constant_Reference(Position)));
					if Current = Position then
						Forms.Write_In_HTML (Output, Form, " *");
					end if;
					String'Write (Output, "</option>");
				end if;
			end if;
		end loop;
		String'Write (Output, "<option value=""-1"" ");
		if Current < 0 then
			Forms.Write_Attribute_Name (Output, "selected");
			Forms.Write_Attribute_Open (Output);
			Forms.Write_In_Attribute (Output, Form, "selected");
			Forms.Write_Attribute_Close (Output);
		end if;
		Character'Write (Output, '>');
		Forms.Write_In_HTML (Output, Form, "棄権");
		if Current < 0 then
			Forms.Write_In_HTML (Output, Form, " *");
		end if;
		String'Write (Output, "</option>" & Line_Break & "</select>" & Line_Break);
		case Kind is
			when Vampire.Villages.Hunter =>
				case Special is
					when Allowed =>
						case Form.Template_Set is
							when Forms.For_Full =>
								String'Write (Output, "<br />");
							when Forms.For_Mobile =>
								Character'Write (Output, ' ');
						end case;
						String'Write(Output, "<input name=""special"" type=""checkbox"" ");
						if Current_Special then
							Forms.Write_Attribute_Name (Output, "checked");
							Forms.Write_Attribute_Open (Output);
							Forms.Write_In_Attribute (Output, Form, "checked");
							Forms.Write_Attribute_Close (Output);
						end if;
						String'Write (Output, "/>");
						Forms.Write_In_HTML (Output, Form, "銀の弾丸 ");
					when Disallowed | Already_Used =>
						null;
				end case;
			when others =>
				null;
		end case;
		case Form.Template_Set is
			when Forms.For_Full =>
				String'Write (Output, "</td>" & Line_Break & "<td class=""button""");
				if Keeping_Silver_Bullet then
					String'Write (Output, " rowspan=""2""");
				end if;
				Character'Write (Output, '>');
			when Forms.For_Mobile =>
				null;
		end case;
		String'Write (Output, "<input type=""submit"" ");
		Forms.Write_Attribute_Name (Output, "value");
		Forms.Write_Attribute_Open (Output);
		Forms.Write_In_Attribute (Output, Form, Button);
		Forms.Write_Attribute_Close (Output);
		String'Write (Output, "/>");
		case Form.Template_Set is
			when Forms.For_Full =>
				String'Write (Output, "</td>" & Line_Break & "</tr>");
				if Keeping_Silver_Bullet then
					String'Write (Output, "<tr><td>");
					Forms.Write_In_HTML (Output, Form, "銀の弾丸はまだとっておきましょう。");
					String'Write (Output, "</tr></td>");
				end if;
				String'Write (Output, "</table>" & Line_Break);
			when Forms.For_Mobile =>
				if Keeping_Silver_Bullet then
					String'Write (Output, "<div>");
					Forms.Write_In_HTML (Output, Form, "銀の弾丸はまだとっておきましょう。");
					String'Write (Output, "</div>");
				end if;
		end case;
		String'Write (Output, "<input type=""hidden"" name=""cmd"" ");
		Forms.Write_Attribute_Name (Output, "value");
		Forms.Write_Attribute_Open (Output);
		case Kind is
			when Vampire.Villages.Inhabitant =>
				Forms.Write_In_Attribute (Output, Form, "vote");
			when others =>
				Forms.Write_In_Attribute (Output, Form, "target");
		end case;
		Forms.Write_Attribute_Close (Output);
		String'Write (Output, "/>" & Line_Break & "</form>" & Line_Break);
	end Vote_Form;
	
	function Role_Text(Person : Vampire.Villages.Person_Type) return String is
		Setting : Vampire.Villages.Person_Record renames Person.Records.Constant_Reference(Village.Today);
	begin
		if Setting.State = Vampire.Villages.Died then
			return "あなたは幽霊です。";
		else
			case Person.Role is
				when Vampire.Villages.Inhabitant | Vampire.Villages.Loved_Inhabitant | Vampire.Villages.Unfortunate_Inhabitant =>
					return "あなたは村人です。";
				when Vampire.Villages.Doctor =>
					if Setting.Target >= 0 then
						return "あなたは医者、" & Villages.Text.Name(Village.People.Constant_Reference(Setting.Target)) & "を診察しました。";
					else
						return "あなたは医者です。";
					end if;
				when Vampire.Villages.Detective =>
					if Setting.Target >= 0 then
						return "あなたは探偵、" & Villages.Text.Name(Village.People.Constant_Reference(Setting.Target)) & "を調査中です。";
					else
						return "あなたは探偵です。";
					end if;
				when Vampire.Villages.Astronomer =>
					if Person.Commited and then Setting.Target >= 0 then
						return "あなたは天文家、" & Villages.Text.Name(Village.People.Constant_Reference(Setting.Target)) & "の家の空を観測します。";
					else
						return "あなたは天文家です。";
					end if;
				when Vampire.Villages.Hunter =>
					if Person.Commited and then Setting.Target >= 0 and then Setting.Special then
						return "あなたは猟師、" & Villages.Text.Name(Village.People.Constant_Reference(Setting.Target)) & "を銀の弾丸で守ります。";
					elsif Person.Commited and then Setting.Target >= 0 then
						return "あなたは猟師、" & Villages.Text.Name(Village.People.Constant_Reference(Setting.Target)) & "を守ります。";
					elsif Person.Commited and then Setting.Special then
						return "あなたは猟師、銃には銀の弾丸を装填しています。";
					else
						return "あなたは猟師です。";
					end if;
				when Vampire.Villages.Lover =>
					for Position in Village.People.First_Index .. Village.People.Last_Index loop
						if Village.People.Constant_Reference(Position).Role = Vampire.Villages.Loved_Inhabitant then
							return "あなたは" & Villages.Text.Name(Village.People.Constant_Reference(Position)) & "に片想いです。";
						end if;
					end loop;
					pragma Assert(False);
					return "";
				when Vampire.Villages.Sweetheart_M | Vampire.Villages.Sweetheart_F =>
					for Position in Village.People.First_Index .. Village.People.Last_Index loop
						if Village.People.Constant_Reference(Position).Role /= Person.Role
							and then Village.People.Constant_Reference(Position).Role in Vampire.Villages.Sweetheart_M .. Vampire.Villages.Sweetheart_F
						then
							return "あなたは" & Villages.Text.Name(Village.People.Constant_Reference(Position)) & "の恋人です。";
						end if;
					end loop;
					pragma Assert(False);
					return "";
				when Vampire.Villages.Servant =>
					return "あなたは吸血鬼に身を捧げることが喜びである使徒です。";
				when Vampire.Villages.Vampire_Role =>
					declare
						Mark : constant array(Vampire.Villages.Vampire_Role) of Character := ('K', 'Q', 'J');
					begin
						if Person.Commited and then Setting.Target >= 0 then
							return "あなたは吸血鬼(" & Mark(Person.Role) & ")、" & Villages.Text.Name(Village.People.Constant_Reference(Setting.Target)) & "を襲います。";
						else
							return "あなたは吸血鬼(" & Mark(Person.Role) & ")です。";
						end if;
					end;
				when Vampire.Villages.Gremlin =>
					return "あなたは妖魔です。";
			end case;
		end if;
	end Role_Text;
	
	Player_Index : constant Integer := Vampire.Villages.Joined (Village, User_Id);
	
	Message_Range : constant Tabula.Villages.Message_Range_Type :=
		Village.Message_Range (Day);
	Message_Counts : Vampire.Villages.Message_Counts renames Vampire.Villages.Count_Messages (Village, Day);
	Tip_Showed : Boolean := False;
	
	type Paging_Pos is (Top, Bottom, Tip);
	
	procedure Paging (
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Pos : Paging_Pos)
	is
		F : Natural;
		L : Integer;
	begin
		if Pos /= Tip then
			F := Integer'Max (Message_Range.First, Showing_Range.First);
			L := Integer'Min (Message_Range.Last, Showing_Range.Last);
			if F = Message_Range.First and then L = Message_Range.Last then
				String'Write (Output, "<hr><div>");
				Forms.Write_In_HTML (Output, Form, "全");
			else
				String'Write (Output, "<hr><div><a ");
				Forms.Write_Attribute_Name (Output, "href");
				Forms.Write_Link (
					Output,
					Form,
					Current_Directory => Current_Directory,
					Resource => Forms.Self,
					Parameters => Form.Parameters_To_Village_Page (
						Village_Id => Village_Id,
						Day => Day,
						First => Message_Range.First,
						Last => Integer'Max (Message_Range.First, Message_Range.Last),
						User_Id => User_Id,
						User_Password => User_Password));
				Character'Write (Output, '>');
				Forms.Write_In_HTML (Output, Form, "全");
				String'Write (Output, "</a>");
			end if;
			for I in 0 .. (Message_Range.Last - Message_Range.First) / Form.Speeches_Per_Page loop
				declare
					I_S : constant String := Image (I + 1);
					I_F : constant Natural := I * Form.Speeches_Per_Page;
					I_L : Integer := Natural'Min (Message_Range.Last, I_F + (Form.Speeches_Per_Page - 1));
				begin
					if F = I_F and then L = I_L then
						Forms.Write_In_HTML (Output, Form, "|" & I_S);
					else
						Forms.Write_In_HTML (Output, Form, "|");
						String'Write (Output, "<a ");
						Forms.Write_Attribute_Name (Output, "href");
						Forms.Write_Link (
							Output,
							Form,
							Current_Directory => Current_Directory,
							Resource => Forms.Self,
							Parameters => Form.Parameters_To_Village_Page (
								Village_Id => Village_Id,
								Day => Day,
								First => I_F,
								Last => I_L,
								User_Id => User_Id,
								User_Password => User_Password));
						Character'Write (Output, '>');
						Forms.Write_In_HTML (Output, Form, I_S);
						String'Write (Output, "</a>");
					end if;
				end;
			end loop;
			if Village.State = Closed or else Day /= Village.Today then
				Forms.Write_In_HTML (Output, Form, "|");
			elsif F = Integer'Max (Message_Range.First, Message_Range.Last - (Form.Speeches_Per_Page - 1))
				and then L = Message_Range.Last
			then
				Forms.Write_In_HTML (Output, Form, "|新|");
			else
				Forms.Write_In_HTML (Output, Form, "|");
				String'Write (Output, "<a ");
				Forms.Write_Attribute_Name (Output, "href");
				Forms.Write_Link (
					Output,
					Form,
					Current_Directory => Current_Directory,
					Resource => Forms.Self,
					Parameters => Form.Parameters_To_Village_Page (
						Village_Id => Village_Id,
						Day => Day,
						Latest => Form.Speeches_Per_Page,
						User_Id => User_Id,
						User_Password => User_Password));
				Character'Write (Output, '>');
				Forms.Write_In_HTML (Output, Form, "新");
				String'Write (Output, "</a>");
				Forms.Write_In_HTML (Output, Form, "|");
			end if;
		else
			String'Write (Output, "<hr><div>");
			Forms.Write_In_HTML (Output, Form, "末端です……");
		end if;
		if Pos = Top then
			String'Write (Output, "<a name=""top"" href=""#bottom"">");
			Forms.Write_In_HTML (Output, Form, "下");
			String'Write (Output, "</a></div>");
		else
			String'Write (Output, "<a name=""bottom"" href=""#top"">");
			Forms.Write_In_HTML (Output, Form, "上");
			String'Write (Output, "</a></div>");
		end if;
	end Paging;
	
	procedure Handle (
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Tag : in String;
		Template : in Web.Producers.Template) is
	begin
		if Tag = "userpanel" then
			if not Log then
				Handle_User_Panel (
					Output,
					Template,
					Form,
					User_Id => User_Id,
					User_Password => User_Password);
			end if;
		elsif Tag = "href_stylesheet" then
			Forms.Write_Attribute_Name (Output, "href");
			Forms.Write_Link (
				Output,
				Form,
				Current_Directory => Current_Directory,
				Resource => Style_Sheet);
		elsif Tag = "villagename" then
			Forms.Write_In_HTML (
				Output,
				Form,
				Village.Name.Constant_Reference & ' ' &
				Day_Name (Day, Village.Today, Village.State));
		elsif Tag = "background" then
			Forms.Write_Attribute_Name (Output, "background");
			Forms.Write_Link (
				Output,
				Form,
				Current_Directory => Current_Directory,
				Resource => Background);
		elsif Tag = "styles" then
			if not Village.People.Is_Empty then
				String'Write (Output, "<style>");
				for I in Village.People.First_Index .. Village.People.Last_Index loop
					String'Write (Output, ".p" & Image (I) & "{display:none;} ");
				end loop;
				String'Write (Output, ".pg{display:none;} ");
				String'Write (Output, "</style>");
				for I in Village.People.First_Index .. Village.People.Last_Index loop
					String'Write (Output, "<style id=""s" & Image (I) & """>.p" & Image (I) & "{display:block;} </style>");
				end loop;
				String'Write (Output, "<style id=""sg"">.pg{display:block;} </style>");
			end if;
			-- ここでやるべきでもないがついでに
			if Village.State = Playing
				and then Village.Term = Short
				and then Form.Template_Set = Forms.For_Full
			then
				Next_Dawn : declare
					Next : Ada.Calendar.Time;
					Y : Ada.Calendar.Year_Number;
					M : Ada.Calendar.Month_Number;
					D : Ada.Calendar.Day_Number;
					H : Ada.Calendar.Formatting.Hour_Number;
					N : Ada.Calendar.Formatting.Minute_Number;
					S : Ada.Calendar.Formatting.Second_Number;
					T : Ada.Calendar.Formatting.Second_Duration;
				begin
					case Village.Time is
						when Night => Next := Village.Night_To_Daytime;
						when Daytime => Next := Village.Daytime_To_Vote;
						when Vote => Next := Village.Vote_To_Night;
					end case;
					Ada.Calendar.Formatting.Split(Next, Y, M, D, H, N, S, T, Time_Zone => Calendar.Time_Offset);
					declare
						Y : String renames Ada.Calendar.Year_Number'Image(Next_Dawn.Y);
						M : String renames Ada.Calendar.Month_Number'Image(Next_Dawn.M);
						D : String renames Ada.Calendar.Day_Number'Image(Next_Dawn.D);
						H : String renames Ada.Calendar.Formatting.Hour_Number'Image(Next_Dawn.H);
						N : String renames Ada.Calendar.Formatting.Minute_Number'Image(Next_Dawn.N);
						S : String renames Ada.Calendar.Formatting.Second_Number'Image(Next_Dawn.S);
					begin
						String'Write (Output, Line_Break &
							"<script type=""text/javascript"">" &
							"var limit = new Date(" & Y & "," & M & " - 1," & D & "," & H & "," & N & "," & S & ");" &
							"var timer = setTimeout('on_time()', 500);" &
							"function on_time(){" &
							"clearTimeout(timer);" &
							"var t = (limit.getTime() - (new Date).getTime()) / 1000;" &
							"var m = document.getElementById('min');" &
							"if(m) m.firstChild.nodeValue = Math.floor(t / 60);" &
							"var s = document.getElementById('sec');" &
							"if(s) s.firstChild.nodeValue = Math.floor(t % 60);" &
							"timer = setTimeout('on_time()', 500);" &
							"}" &
							"</script>");
					end;
				end Next_Dawn;
			end if;
		elsif Tag = "days" then
			for I in 0 .. Village.Today loop
				declare
					procedure Handle_Days (
						Output : not null access Ada.Streams.Root_Stream_Type'Class;
						Tag : in String;
						Template : in Web.Producers.Template) is
					begin
						if Tag = "today" then
							if I = Day then
								Web.Producers.Produce (Output, Template, Handler => Handle_Days'Access);
							end if;
						elsif Tag = "otherday" then
							if I /= Day then
								Web.Producers.Produce (Output, Template, Handler => Handle_Days'Access);
							end if;
						elsif Tag = "day" then
							Forms.Write_In_HTML (Output, Form, Day_Name (I, Village.Today, Village.State));
						elsif Tag = "href_day" then
							Forms.Write_Attribute_Name (Output, "href");
							Forms.Write_Link_To_Village_Page (
								Output,
								Form,
								Current_Directory => Current_Directory,
								HTML_Directory => HTML_Directory,
								Log => Log,
								Village_Id => Village_Id,
								Day => I,
								User_Id => User_Id,
								User_Password => User_Password);
						else
							raise Program_Error with "Invalid template """ & Tag & """";
						end if;
					end Handle_Days;
				begin
					Web.Producers.Produce (Output, Template, Handler => Handle_Days'Access);
				end;
			end loop;
		elsif Tag = "summary" then
			declare
				procedure Handle_Summary (
					Output : not null access Ada.Streams.Root_Stream_Type'Class;
					Tag : in String;
					Template : in Web.Producers.Template) is
				begin
					if Tag = "person" then
						for I in Village.People.First_Index .. Village.People.Last_Index loop
							declare
								Person : Vampire.Villages.Person_Type renames Village.People.Constant_Reference(I);
								procedure Handle_Person (
									Output : not null access Ada.Streams.Root_Stream_Type'Class;
									Tag : in String;
									Template : in Web.Producers.Template) is
								begin
									if Tag = "for_cn" then
										Forms.Write_Attribute_Name (Output, "for");
										Forms.Write_Attribute_Open (Output);
										Forms.Write_In_Attribute (Output, Form, 'c' & Image (I));
										Forms.Write_Attribute_Close (Output);
									elsif Tag = "id_cn" then
										Forms.Write_Attribute_Name (Output, "id");
										Forms.Write_Attribute_Open (Output);
										Forms.Write_In_Attribute (Output, Form, 'c' & Image (I));
										Forms.Write_Attribute_Close (Output);
									elsif Tag = "onclick" then
										Forms.Write_Attribute_Name (Output, "onclick");
										Forms.Write_Attribute_Open (Output);
										Forms.Write_In_Attribute (Output, Form, "javascript:sync(" & Image (I) & ")");
										Forms.Write_Attribute_Close (Output);
									elsif Tag = "name" then
										Forms.Write_In_HTML (Output, Form, Villages.Text.Name (Person));
									elsif Tag = "speech" then
										Forms.Write_In_HTML (Output, Form, Image (Message_Counts(I).Speech));
										if Message_Counts(I).Encouraged > 0 then
											String'Write (Output, " <small>/");
											Forms.Write_In_HTML (Output, Form, Image (Speech_Limit + Message_Counts(I).Encouraged * Encouraged_Speech_Limit));
											String'Write (Output, "</small>");
										end if;
									elsif Tag = "administrator" then
										if User_id = Tabula.Users.Administrator then
											Web.Producers.Produce(Output, Template, Handler => Handle_Person'Access);
										end if;
									elsif Tag = "id" then
										Forms.Write_In_HTML (Output, Form, Person.Id.Constant_Reference);
									elsif Tag = "remove" then
										if Village.State = Prologue then
											Web.Producers.Produce(Output, Template, Handler => Handle_Person'Access);
										end if;
									elsif Tag = "value_target" then
										Forms.Write_Attribute_Name (Output, "value");
										Forms.Write_Attribute_Open (Output);
										Forms.Write_In_Attribute (Output, Form, Image (I));
										Forms.Write_Attribute_Close (Output);
									else
										raise Program_Error with "Invalid template """ & Tag & """";
									end if;
								end Handle_Person;
							begin
								if (Village.State >= Epilogue and then Village.Today = Day)
									or else Person.Records.Constant_Reference(Day).State /= Vampire.Villages.Died
								then
									Web.Producers.Produce(Output, Template, Handler => Handle_Person'Access);
								end if;
							end;
						end loop;
					elsif Tag = "ghost-filter" then
						if Village.Is_Anyone_Died (Day)
							and then (Village.State < Epilogue or else Day < Village.Today)
							and then (
								Village.State >= Epilogue
								or else (
									Player_Index /= No_Person
									and then Village.People.Constant_Reference(Player_Index).
										Records.Constant_Reference(Village.Today).State = Vampire.Villages.Died))
						then
							Web.Producers.Produce(Output, Template);
						end if;
					else
						raise Program_Error with "Invalid template """ & Tag & """";
					end if;
				end Handle_Summary;
			begin
				if not Village.People.Is_Empty then
					Web.Producers.Produce(Output, Template, Handler => Handle_Summary'Access);
				end if;
			end;
		elsif Tag = "rule" then
			if Day = 0 then
				Rule_Panel (
					Output => Output,
					Template => Template,
					Village_Id => Village_Id,
					Village => Village,
					Player => Player_Index >= 0,
					User_Id => User_Id,
					User_Password => User_Password);
			end if;
		elsif Tag = "roleset" then
			if Day = 0 and then Village.State <= Playing and then Village.People.Length >= 3 then
				declare
					Sets : constant Vampire.Villages.Teaming.Role_Set_Array :=
						Vampire.Villages.Teaming.Possibilities (
							People_Count => Village.People.Length,
							Male_And_Female => Village.Male_And_Female,
							Execution => Village.Execution,
							Formation => Village.Formation,
							Unfortunate => Village.Unfortunate,
							Monster_Side => Village.Monster_Side);
					procedure Handle_Role_Set (
						Output : not null access Ada.Streams.Root_Stream_Type'Class;
						Tag : in String;
						Template : in Web.Producers.Template) is
					begin
						if Tag = "items" then
							for I in Sets'Range loop
								declare
									procedure Handle_Item (
										Output : not null access Ada.Streams.Root_Stream_Type'Class;
										Tag : in String;
										Template : in Web.Producers.Template) is
									begin
										if Tag = "set" then
											for J in Vampire.Villages.Person_Role loop
												for K in 1 .. Sets (I)(J) loop
													Forms.Write_In_HTML (Output, Form, Villages.Text.Short_Image (J));
												end loop;
											end loop;
										else
											raise Program_Error with "Invalid template """ & Tag & """";
										end if;
									end Handle_Item;
								begin
									Web.Producers.Produce (Output, Template, Handler => Handle_Item'Access);
								end;
							end loop;
						else
							raise Program_Error with "Invalid template """ & Tag & """";
						end if;
					end Handle_Role_Set;
				begin
					Web.Producers.Produce (Output, Template, Handler => Handle_Role_Set'Access);
				end;
			end if;
		elsif Tag = "href_index" then
			Forms.Write_Attribute_Name (Output, "href");
			Forms.Write_Link (
				Output,
				Form,
				Current_Directory => Current_Directory,
				Resource => Forms.Self,
				Parameters => Form.Parameters_To_Index_Page (
					User_Id => User_Id,
					User_Password => User_Password));
		elsif Tag = "all" then
			if Showing_Range.First > Message_Index'First then
				declare
					procedure Handle_Range_All (
						Output : not null access Ada.Streams.Root_Stream_Type'Class;
						Tag : in String;
						Template : in Web.Producers.Template) is
					begin
						if Tag = "href_all" then
							Forms.Write_Attribute_Name (Output, "href");
							Forms.Write_Link (
								Output,
								Form,
								Current_Directory => Current_Directory,
								Resource => Forms.Self,
								Parameters => Form.Parameters_To_Village_Page (
									Village_Id => Village_Id,
									Day => Day,
									First => Tabula.Villages.Message_Index'First,
									User_Id => User_Id,
									User_Password => User_Password));
						else
							raise Program_Error with "Invalid template """ & Tag & """";
						end if;
					end Handle_Range_All;
				begin
					Web.Producers.Produce (Output, Template, Handler => Handle_Range_All'Access);
				end;
			end if;
		elsif Tag = "message" then
			declare
				procedure Narration (
					Message : String;
					Class : String := "narration";
					Role : Vampire.Villages.Person_Role := Vampire.Villages.Inhabitant)
				is
					procedure Handle_Narration (
						Output : not null access Ada.Streams.Root_Stream_Type'Class;
						Tag : in String;
						Template : in Web.Producers.Template) is
					begin
						if Tag = "text" then
							Forms.Write_In_HTML (Output, Form, Message);
						elsif Tag = "roleimg" then
							if Role /= Inhabitant then
								pragma Assert (Class = "narrationi");
								Web.Producers.Produce (Output, Template, Handler => Handle_Narration'Access);
							end if;
						elsif Tag = "src_roleimg" then
							Forms.Write_Attribute_Name (Output, "src");
							Forms.Write_Link (
								Output,
								Form,
								Current_Directory => Current_Directory,
								Resource => Ada.Hierarchical_File_Names.Compose (
									Directory => Image_Directory,
									Relative_Name => Relative_Role_Images (Role).all));
						else
							raise Program_Error with "Invalid template """ & Tag & """";
						end if;
					end Handle_Narration;
				begin
					if Message'Length > 0 then
						Web.Producers.Produce(Output, Template, Class, Handler => Handle_Narration'Access);
					end if;
				end Narration;
				procedure Speech (
					Message : Vampire.Villages.Message;
					Filter_Subject : Person_Index'Base;
					Time : Ada.Calendar.Time;
					Class : String;
					X : Integer := -1)
				is
					Filter : aliased Ada.Strings.Unbounded.Unbounded_String;
					Subject_Ref : access constant Villages.Person_Type;
				begin
					if Message.Kind = Escaped_Speech then
						Subject_Ref := Village.Escaped_People.Constant_Reference (Message.Subject).Element;
					elsif Message.Kind in Detective_Message_Kind then
						Subject_Ref := Village.People.Constant_Reference (Message.Target).Element;
					else
						Subject_Ref := Village.People.Constant_Reference (Message.Subject).Element;
					end if;
					if X >= 0 then
						Ada.Strings.Unbounded.Append (Filter, "s");
						Ada.Strings.Unbounded.Append (Filter, Image (X));
						Ada.Strings.Unbounded.Append (Filter, ' ');
					end if;
					if Message.Kind = Vampire.Villages.Ghost then
						Ada.Strings.Unbounded.Append (Filter, "pg");
					elsif Filter_Subject /= No_Person then
						Ada.Strings.Unbounded.Append (Filter, "p");
						Ada.Strings.Unbounded.Append (Filter, Image (Filter_Subject));
					end if;
					R3.Handle_Speech (
						Output,
						Template,
						Class,
						Form,
						Current_Directory => Current_Directory,
						Image_Directory => Image_Directory,
						Face_Width => Village.Face_Width,
						Face_Height => Village.Face_Height,
						Subject => Subject_Ref.all,
						Time => Time,
						Text => Message.Text.Constant_Reference,
						Filter => Filter.Constant_Reference);
				end Speech;
				procedure Note (Subject : Vampire.Villages.Person_Type; Rec : Vampire.Villages.Person_Record; Class : String) is
					Silence : aliased constant String := "……。";
					Text : access constant String := Rec.Note.Constant_Reference.Element;
				begin
					if Text.all = "" then
						Text := Silence'Access;
					end if;
					R3.Handle_Speech (
						Output,
						Template,
						Class,
						Form,
						Current_Directory => Current_Directory,
						Image_Directory => Image_Directory,
						Face_Width => Village.Face_Width,
						Face_Height => Village.Face_Height,
						Subject => Subject,
						Time => Tabula.Calendar.Null_Time,
						Text => Text.all,
						Filter => "");
				end Note;
				subtype X_Type is Integer range 1 .. 3;
				function Random_X is
					new Ada.Numerics.Distributions.Linear_Discrete_Random (
						Ada.Numerics.MT19937.Cardinal,
						X_Type,
						Ada.Numerics.MT19937.Generator,
						Ada.Numerics.MT19937.Random_32);
				Executed : Integer := -1;
				Speech_Index : Tabula.Villages.Message_Index := Tabula.Villages.Message_Index'First;
				X : X_Type := 2;
				Last_Speech : Integer := -1;
				Last_Speech_Time : Ada.Calendar.Time := Calendar.Null_Time;
				X_Generator : aliased Ada.Numerics.MT19937.Generator :=
					Ada.Numerics.MT19937.Initialize (12);
			begin
				if Form.Paging then
					Paging (Output, Top);
				end if;
				for Position in Village.Messages.First_Index .. Village.Messages.Last_Index loop
					declare
						Message : Vampire.Villages.Message renames Village.Messages.Constant_Reference(Position);
					begin
						if Message.Day = Day then
							if Speech_Index in Showing_Range.First .. Showing_Range.Last + 1 then
								case Message.Kind is
									when Vampire.Villages.Narration =>
										Narration (Message.Text.Constant_Reference);
									when Vampire.Villages.Escape =>
										Narration (
											Villages.Text.Escape (Village, Message),
											Class => "narratione");
									when Vampire.Villages.Join =>
										Narration (Villages.Text.Join (Village, Message));
									when Vampire.Villages.Escaped_Join =>
										Narration (
											Villages.Text.Escaped_Join (Village, Message),
											Class => "narratione");
									when Vampire.Villages.Speech | Vampire.Villages.Escaped_Speech =>
										exit when Speech_Index > Showing_Range.Last;
										declare
											Subject : Person_Index'Base := Message.Subject;
										begin
											if Message.Kind = Escaped_Speech then
												Subject := Village.Joined (
													Village.Escaped_People.Constant_Reference (Message.Subject).
														Id.Constant_Reference);
												if Subject /= No_Person
													and then not Same_Id_And_Figure (
														Village.Escaped_People.Constant_Reference (Message.Subject),
														Village.People.Constant_Reference (Subject))
												then
													Subject := No_Person;
												end if;
											end if;
											if Subject /= No_Person then
												if Last_Speech /= Subject then
													New_X : loop
														declare
															X2 : constant X_Type := Random_X (X_Generator);
														begin
															if X2 /= X then
																X := X2;
																exit New_X;
															end if;
														end;
													end loop New_X;
													Last_Speech := Subject;
													if Message.Time - Last_Speech_Time < Speech_Simultaneous then
														declare
															Uso : constant Duration := Duration(Long_Long_Integer(Message.Time - Calendar.Null_Time) rem 3);
														begin
															Last_Speech_Time := Last_Speech_Time + Speech_Simultaneous + Uso;
														end;
													else
														Last_Speech_Time := Message.Time;
													end if;
												else
													Last_Speech_Time := Message.Time;
												end if;
												Speech (Message, Subject, Last_Speech_Time, "speech", X => X);
											else
												Speech (Message, No_Person, Message.Time, "escaped");
											end if;
										end;
									when Vampire.Villages.Monologue =>
										if Village.State >= Epilogue
											or else Message.Subject = Player_Index
										then
											Speech (Message, Message.Subject, Message.Time, "monologue");
										end if;
									when Vampire.Villages.Ghost =>
										if Village.State >= Epilogue
											or else (Player_Index >= 0 and then Village.People.Constant_Reference(Player_Index).Records.Constant_Reference(Village.Today).State = Vampire.Villages.Died)
										then
											Speech (Message, Message.Subject, Message.Time, "ghost");
										end if;
									when Vampire.Villages.Howling =>
										if Village.State >= Epilogue
											or else (Player_Index >= 0 and then Village.People.Constant_Reference(Player_Index).Role in Vampire.Villages.Vampire_Role)
										then
											Speech (Message, Message.Subject, Message.Time, "vampire");
										end if;
									when Vampire.Villages.Howling_Blocked =>
										if Village.State >= Epilogue
											or else (Player_Index >= 0 and then Village.People.Constant_Reference(Player_Index).Role in Vampire.Villages.Vampire_Role)
										then
											Narration (
												Villages.Text.Howling_Blocked (Village),
												"narrationi",
												Vampire.Villages.Vampire_K);
										end if;
									when Vampire.Villages.Action_Wake =>
										Narration (Villages.Text.Action_Wake (Village, Message));
									when Vampire.Villages.Action_Encourage =>
										Narration (Villages.Text.Action_Encourage (Village, Message));
									when Vampire.Villages.Action_Vampire_Gaze =>
										if Village.State >= Epilogue
											or else (
												Player_Index >= 0
												and then Village.People.Constant_Reference (Player_Index).Role in
													Vampire.Villages.Vampire_Role)
										then
											Narration (
												Villages.Text.Action_Vampire_Gaze (Village, Message),
												"narrationi",
												Vampire.Villages.Vampire_K);
										end if;
									when Vampire.Villages.Action_Vampire_Gaze_Blocked =>
										if Village.State >= Epilogue
											or else (
												Player_Index >= 0
												and then Village.People.Constant_Reference (Player_Index).Role in
													Vampire.Villages.Vampire_Role)
										then
											Narration(
												Villages.Text.Action_Vampire_Gaze_Blocked (Village, Message),
												"narrationi",
												Vampire.Villages.Vampire_K);
										end if;
									when Action_Vampire_Cancel =>
										if Village.State >= Epilogue or else Player_Index = Message.Subject then
											Narration (
												Villages.Text.Action_Vampire_Cancel (Village, Message),
												"narrationi",
												Vampire_K);
										end if;
									when Action_Vampire_Canceled =>
										if Village.State >= Epilogue or else Player_Index = Message.Subject then
											Narration (
												Villages.Text.Action_Vampire_Canceled (Village, Message),
												"narrationi",
												Vampire_K);
										end if;
									when Vampire.Villages.Servant_Message_Kind =>
										if Village.State >= Epilogue or else Player_Index = Message.Subject then
											Narration (
												Villages.Text.Servant_Knew_Message (Village, Message),
												"narrationi",
												Vampire.Villages.Servant);
										end if;
									when Vampire.Villages.Doctor_Message_Kind =>
										if Village.State >= Epilogue or else (Player_Index = Message.Subject) then
											Narration (
												Villages.Text.Doctor_Cure_Message (Village, Message),
												"narrationi",
												Vampire.Villages.Doctor);
										end if;
									when Vampire.Villages.Detective_Message_Kind =>
										if Village.State >= Epilogue or else (Player_Index = Message.Subject) then
											Narration (
												Villages.Text.Detective_Survey_Message (Village, Message),
												"narrationi",
												Vampire.Villages.Detective);
											if not Message.Text.Is_Null
												and then (
													Village.Daytime_Preview = Vampire.Villages.Role_And_Message
													or else Village.Daytime_Preview = Vampire.Villages.Message_Only
													or else Message.Kind /= Vampire.Villages.Detective_Survey_Preview)
											then
												Speech (Message, No_Person, Message.Time, "dying");
											end if;
										end if;
									when Preliminary_Vote =>
										Narration (
											Villages.Text.Votes (
												Village,
												Day => Message.Day,
												Preliminary => True,
												Player_Index => -1));
										Narration (
											Villages.Text.Votes_Totaled (
												Village,
												Day => Message.Day,
												Preliminary => True,
												Executed => -1));
									when Vampire.Villages.Execution =>
										case Village.Vote is
											when Unsigned =>
												Narration (
													Villages.Text.Votes (
														Village,
														Day => Message.Day - 1,
														Preliminary => False,
														Player_Index => Player_Index),
													"narrationi");
											when Preliminary_And_Final =>
												Narration (
													Villages.Text.Votes (
														Village,
														Day => Message.Day - 1,
														Preliminary => False,
														Player_Index => -1));
										end case;
										Narration (
											Villages.Text.Votes_Totaled (
												Village,
												Day => Message.Day - 1,
												Preliminary => False,
												Executed => Message.Target));
										Executed := Message.Target;
									when Vampire.Villages.Awareness =>
										if Village.State >= Epilogue or else Player_Index = Message.Subject then
											Narration (
												Villages.Text.Awareness (Village, Message),
												"narrationi",
												Village.People.Constant_Reference (Message.Subject).Role);
										end if;
									when Vampire.Villages.Astronomer_Observation =>
										if Village.State >= Epilogue or else Player_Index = Message.Subject then
											Narration (
												Villages.Text.Astronomer_Observation_Message (Village, Message),
												"narrationi",
												Vampire.Villages.Astronomer);
										end if;
									when Vampire.Villages.Hunter_Message_Kind =>
										if Village.State >= Tabula.Villages.Epilogue or else Player_Index = Message.Subject then
											Narration (
												Villages.Text.Hunter_Guard_Message (Village, Message),
												"narrationi",
												Vampire.Villages.Hunter);
										end if;
									when Vampire.Villages.Meeting => null;
										if Village.State >= Epilogue
											or else (Player_Index >= 0 and then Village.People.Constant_Reference(Player_Index).Role in Vampire.Villages.Vampire_Role)
										then
											for Role in Vampire.Villages.Vampire_Role loop
												for I in Village.People.First_Index .. Village.People.Last_Index loop
													declare
														Person : Vampire.Villages.Person_Type renames Village.People.Constant_Reference(I);
													begin
														if Person.Role = Role then
															declare
																Yesterday_Record : Vampire.Villages.Person_Record renames Person.Records.Constant_Reference(Day - 1);
															begin
																if Yesterday_Record.State /= Vampire.Villages.Died and then Executed /= I then
																	Note(Person, Yesterday_Record, "vampire");
																end if;
															end;
														end if;
													end;
												end loop;
											end loop;
										end if;
									when Vampire.Villages.Vampire_Message_Kind =>
										if Village.State >= Epilogue
											or else (Player_Index /= No_Person
												and then (Message.Subject = Player_Index
													or else (Village.People.Constant_Reference (Message.Subject).Role in Vampire_Role
														and then Village.People.Constant_Reference (Player_Index).Role in Vampire_Role)))
										then
											Narration (
												Villages.Text.Vampire_Murder_Message (Village, Message, Executed),
												"narrationi",
												Vampire.Villages.Vampire_K);
										end if;
									when Vampire.Villages.Gremlin_Sense =>
										if Village.State >= Epilogue or else (Player_Index = Message.Subject) then
											Narration (
												Villages.Text.Gremlin_Sense (Village, Message.Day),
												"narrationi",
												Vampire.Villages.Gremlin);
										end if;
									when Vampire.Villages.Sweetheart_Incongruity =>
										if Village.State >= Epilogue or else (Player_Index = Message.Subject) then
											Narration (
												Villages.Text.Sweetheart_Incongruity (Village, Message),
												"narrationi",
												Vampire.Villages.Lover);
										end if;
									when Vampire.Villages.Sweetheart_Suicide =>
										if Village.State >= Epilogue or else (Player_Index = Message.Subject) or else (Player_Index = Message.Target) then
											Narration(
												Villages.Text.Sweetheart_Suicide (Village, Message),
												"narrationi",
												Vampire.Villages.Lover);
										end if;
									when Vampire.Villages.List =>
										if Village.Today = Message.Day and then Village.State >= Epilogue then
											Narration (Villages.Text.Fatalities (Village, Message.Day, Executed));
											Narration (Villages.Text.People_In_Epilogure (Village));
											Narration (Villages.Text.Result_In_Epilogure (Village));
										else
											declare
												Log : aliased Ada.Strings.Unbounded.Unbounded_String;
											begin
												Ada.Strings.Unbounded.Append (Log, Villages.Text.Fatalities (Village, Message.Day, Executed));
												if not Log.Is_Null then
													Ada.Strings.Unbounded.Append (Log, Line_Break);
												end if;
												Ada.Strings.Unbounded.Append (Log, Villages.Text.Survivors (Village, Message.Day));
												Narration (Log.Constant_Reference);
											end;
											if Message.Day = 2 then
												Narration (Villages.Text.For_Execution_In_Second (Village));
											end if;
										end if;
									when Foreboding =>
										Narration (Villages.Text.Foreboding_About_Infection_In_First (Village));
									when Vampire.Villages.Introduction =>
										Narration (Villages.Text.Introduction (Village));
									when Vampire.Villages.Breakdown =>
										if Village.State >= Epilogue
											or else (Player_Index >= 0 and then Village.People.Constant_Reference(Player_Index).Role in Vampire.Villages.Vampire_Role)
										then
											Narration (
												Villages.Text.Vampires (Village),
												"narrationi",
												Vampire.Villages.Vampire_K);
										end if;
										Narration (Villages.Text.Breakdown (Village));
										Narration (Villages.Text.Teaming (Village));
								end case;
							end if;
							if Message.Kind = Vampire.Villages.Speech or else Message.Kind = Vampire.Villages.Escaped_Speech then
								Speech_Index := Speech_Index + 1;
							end if;
						end if;
					end;
				end loop;
				if Speech_Index = Message_Range.Last + 1 then
					Tip_Showed := True;
					if Village.State >= Epilogue and then Day < Village.Today then
						for I in Village.People.First_Index .. Village.People.Last_Index loop
							declare
								Subject : Vampire.Villages.Person_Type renames Village.People.Constant_Reference(I);
								Rec : Vampire.Villages.Person_Record renames Subject.Records.Constant_Reference(Day);
							begin
								if Rec.State = Vampire.Villages.Died and then not Rec.Note.Is_Null then
									Note(Subject, Rec, "dying");
								end if;
							end;
						end loop;
					end if;
					if Day = Village.Today and then Village.State /= Closed then
						declare
							procedure Handle_Guidance (
								Output : not null access Ada.Streams.Root_Stream_Type'Class;
								Tag : in String;
								Template : in Web.Producers.Template) is
							begin
								if Village.State <= Playing then
									declare
										Second : Boolean := False;
									begin
										for I in Village.People.First_Index .. Village.People.Last_Index loop
											declare
												P : Vampire.Villages.Person_Type renames Village.People.Constant_Reference(I);
											begin
												if not P.Commited and then P.Records.Constant_Reference(Village.Today).State /= Vampire.Villages.Died then
													if Second then
														Forms.Write_In_HTML (Output, Form, "、");
													end if;
													Forms.Write_In_HTML (Output, Form, Villages.Text.Name (P));
													Second := True;
												end if;
											end;
										end loop;
										if Second then
											Forms.Write_In_HTML (Output, Form, "が行動しています。 ");
										end if;
									end;
								end if;
								case Village.State is
									when Prologue =>
										if Village.People.Length < Minimum_Number_Of_Persons then
											Forms.Write_In_HTML (Output, Form, Image (Minimum_Number_Of_Persons));
											Forms.Write_In_HTML (Output, Form, "人以上の参加を待っています。");
										else
											Forms.Write_In_HTML (Output, Form, "全員が行動を終えると夜が明けます。");
										end if;
									when Playing =>
										case Village.Time is
											when Daytime =>
												case Village.Vote_State is
													when Disallowed =>
														if Village.Execution = Infection_And_From_First and then Village.Today = 1 then
															Forms.Write_In_HTML (
																Output,
																Form,
																Ada.Calendar.Formatting.Image (Village.Infection_In_First_Time, Time_Zone => Calendar.Time_Offset) &
																"まではとりあえず様子をうかがいましょう。 ");
														end if;
													when Allowed_For_Preliminary =>
														declare
															Open_Time : constant Ada.Calendar.Time := Village.Preliminary_Vote_Time;
														begin
															Forms.Write_In_HTML (
																Output,
																Form,
																Ada.Calendar.Formatting.Image (Open_Time, Time_Zone => Calendar.Time_Offset) &
																"に一次開票します。 ");
															if Ada.Calendar.Clock > Open_Time then
																Forms.Write_In_HTML (
																	Output,
																	Form,
																	"開票時間を過ぎているため候補が出揃ったらすぐに開票します。 ");
															end if;
														end;
													when Allowed =>
														null;
												end case;
												Forms.Write_In_HTML (
													Output,
													Form,
													Ada.Calendar.Formatting.Image (Village.Daytime_To_Vote, Time_Zone => Calendar.Time_Offset) &
													"までに行動を終えてください。 ");
											when Vote =>
												Forms.Write_In_HTML (
													Output,
													Form,
													"全員の投票を待っています。 ");
											when Night =>
												Forms.Write_In_HTML (
													Output,
													Form,
													"夜です。 ");
										end case;
										if Village.Term = Short
											and then Form.Template_Set = Forms.For_Full
										then
											Forms.Write_In_HTML (Output, Form, "あと");
											String'Write (Output, "<span id=""min"">?</span>");
											Forms.Write_In_HTML (Output, Form, "分");
											String'Write (Output, "<span id=""sec"">?</span>");
											Forms.Write_In_HTML (Output, Form, "秒です。");
										end if;
									when Epilogue =>
										declare
											Next_Duration : constant Duration := Duration'Max(
												Village.Day_Duration,
												Epilogue_Min_Duration);
										begin
											Forms.Write_In_HTML (
												Output,
												Form,
												Ada.Calendar.Formatting.Image(Village.Dawn + Next_Duration, Time_Zone => Calendar.Time_Offset));
										end;
										Forms.Write_In_HTML (Output, Form, "まで話すことができます。");
									when Closed => null;
								end case;
							end;
						begin
							Web.Producers.Produce(Output, Template, "narration", Handler => Handle_Guidance'Access);
						end;
					end if;
					if Form.Paging then
						Paging (Output, Village_Page.Tip);
					end if;
				else
					if Form.Paging then
						Paging (Output, Bottom);
					end if;
				end if;
			end;
		elsif Tag = "villagepanel" then
			if ((Player_Index >= 0 or else User_Id = Tabula.Users.Administrator) and then Village.Today = Day)
				or else (User_Id /= "" and then Village.State = Prologue)
			then
				if Village.State = Closed then
					Web.Producers.Produce(Output, Template, "closed");
				elsif Player_Index >= 0 then
					declare
						Person : Villages.Person_Type
							renames Village.People.Constant_Reference (Player_Index);
						Bottom : Boolean := True;
						procedure Handle_Player (
							Output : not null access Ada.Streams.Root_Stream_Type'Class;
							Tag : in String;
							Template : in Web.Producers.Template) is
						begin
							if Tag = "id_bottom" then
								if Bottom then
									Forms.Write_Attribute_Name (Output, "id");
									Forms.Write_Attribute_Open (Output);
									Forms.Write_In_Attribute (Output, Form, "bottom");
									Forms.Write_Attribute_Close (Output);
									Bottom := False;
								end if;
							elsif Tag = "name" then
								Forms.Write_In_HTML (Output, Form, Villages.Text.Name (Person));
							elsif Tag = "speech" then
								if Village.State = Epilogue or else (
									(Village.State = Playing or else Village.State = Prologue)
									and then Village.Time = Daytime
									and then Village.People.Constant_Reference(Player_Index).Records.Constant_Reference(Village.Today).State /= Vampire.Villages.Died
									and then not Person.Commited)
								then
									declare
										Rest : constant Integer := Speech_Limit
											+ Message_Counts (Player_Index).Encouraged * Encouraged_Speech_Limit
											- Message_Counts (Player_Index).Speech;
										procedure Handle_Speech (
											Output : not null access Ada.Streams.Root_Stream_Type'Class;
											Tag : in String;
											Template : in Web.Producers.Template) is
										begin
											if Tag = "count" then
												Forms.Write_In_HTML (Output, Form, Image (Rest));
											elsif Tag = "rest" then
												if Village.State = Playing then
													Web.Producers.Produce(Output, Template, Handler => Handle_Speech'Access);
												end if;
											elsif Tag = "edit" then
												if Editing = Speech then
													Forms.Write_In_HTML (Output, Form, Editing_Text, Pre => True);
												end if;
											else
												Handle_Player(Output, Tag, Template);
											end if;
										end Handle_Speech;
									begin
										if Village.State /= Playing or else Rest > 0 then
											Web.Producers.Produce (Output, Template, Handler => Handle_Speech'Access);
										end if;
									end;
								end if;
							elsif Tag = "monologue" then
								if Village.State = Playing
									and then Village.People.Constant_Reference(Player_Index).Records.Constant_Reference(Village.Today).State /= Vampire.Villages.Died
									and then not Person.Commited
								then
									declare
										Rest : constant Integer := Monologue_Limit - Message_Counts(Player_Index).Monologue;
										procedure Handle_Monologue (
											Output : not null access Ada.Streams.Root_Stream_Type'Class;
											Tag : in String;
											Template : in Web.Producers.Template) is
										begin
											if Tag = "count" then
												Forms.Write_In_HTML (Output, Form, Image (Rest));
											elsif Tag = "rest" then
												Web.Producers.Produce(Output, Template, Handler => Handle_Monologue'Access);
											elsif Tag = "edit" then
												if Editing = Monologue then
													Forms.Write_In_HTML (Output, Form, Editing_Text, Pre => True);
												end if;
											else
												Handle_Player(Output, Tag, Template);
											end if;
										end Handle_Monologue;
									begin
										if Rest > 0 then
											Web.Producers.Produce(Output, Template, Handler => Handle_Monologue'Access);
										end if;
									end;
								end if;
							elsif Tag ="ghost" then
								if Village.State = Playing
									and then Village.People.Constant_Reference(Player_Index).Records.Constant_Reference(Village.Today).State = Vampire.Villages.Died
								then
									declare
										Rest : constant Integer := Ghost_Limit - Message_Counts(Player_Index).Ghost;
										procedure Handle_Ghost (
											Output : not null access Ada.Streams.Root_Stream_Type'Class;
											Tag : in String;
											Template : in Web.Producers.Template) is
										begin
											if Tag = "count" then
												Forms.Write_In_HTML (Output, Form, Image (Rest));
											elsif Tag = "rest" then
												Web.Producers.Produce(Output, Template, Handler => Handle_Ghost'Access);
											elsif Tag = "edit" then
												if Editing = Ghost then
													Forms.Write_In_HTML (Output, Form, Editing_Text, Pre => True);
												end if;
											else
												Handle_Player(Output, Tag, Template);
											end if;
										end Handle_Ghost;
									begin
										if Rest > 0 then
											Web.Producers.Produce(Output, Template, Handler => Handle_Ghost'Access);
										end if;
									end;
								end if;
							elsif Tag ="vampire" then
								if Village.State = Playing and then
									not Person.Commited and then
									Person.Role in Vampire.Villages.Vampire_Role and then (
										Message_Counts(Player_Index).Speech > 0 or else (
											Village.Time = Night and then
											Village.People.Constant_Reference(Player_Index).Records.Constant_Reference(Village.Today).State /= Vampire.Villages.Died
										)
									)
								then
									Web.Producers.Produce(Output, Template, Handler => Handle_Player'Access);
								end if;
							elsif Tag ="dying" then
								if Village.State = Playing
									and then not Person.Commited
									and then Village.People.Constant_Reference(Player_Index).Records.Constant_Reference(Village.Today).State = Vampire.Villages.Died
								then
									Web.Producers.Produce(Output, Template, Handler => Handle_Player'Access);
								end if;
							elsif Tag = "note" then
								if Editing = Howling then
									Forms.Write_In_HTML (Output, Form, Editing_Text, Pre => True);
								else
									Forms.Write_In_HTML (
										Output,
										Form,
										Person.Records.Constant_Reference (Village.Today).Note.Constant_Reference,
										Pre => True);
								end if;
							elsif Tag = "zero" then
								if Village.State = Playing
									and then Village.Time /= Night
									and then not Village.People.Constant_Reference(Player_Index).Commited
									and then Village.People.Constant_Reference(Player_Index).Records.Constant_Reference(Village.Today).State /= Vampire.Villages.Died
									and then Message_Counts(Player_Index).Speech = 0
								then
									Web.Producers.Produce(Output, Template);
								end if;
							elsif Tag = "role" then
								if Village.State = Playing then
									if Village.People.Constant_Reference(Player_Index).Records.Constant_Reference(Village.Today).State /= Vampire.Villages.Died then
										Web.Producers.Produce(Output, Template, Handler => Handle_Player'Access);
									else
										Forms.Write_In_HTML (Output, Form, Role_Text(Person));
									end if;
								end if;
							elsif Tag = "roletext" then
								Forms.Write_In_HTML (Output, Form, Role_Text (Person));
							elsif Tag = "src_roleimg" then
								pragma Assert (Village.People.Constant_Reference(Player_Index).Records.Constant_Reference(Village.Today).State /= Died);
								Forms.Write_Attribute_Name (Output, "src");
								Forms.Write_Link (
									Output,
									Form,
									Current_Directory => Current_Directory,
									Resource => Ada.Hierarchical_File_Names.Compose (
										Directory => Image_Directory,
										Relative_Name => Relative_Role_Images (Village.People.Constant_Reference (Player_Index).Role).all));
							elsif Tag = "vote" then
								if Village.State = Playing
									and then Message_Counts(Player_Index).Speech > 0
									and then Village.Vote_State /= Disallowed
								then
									if Person.Commited then
										declare
											Setting : Vampire.Villages.Person_Record
												renames Person.Records.Constant_Reference(Village.Today);
										begin
											String'Write (Output, "<div>");
											if Setting.Vote < 0 then
												Forms.Write_In_HTML (Output, Form, "処刑を選ぶ投票は棄権します。");
											else
												declare
													Target : Vampire.Villages.Person_Type
														renames Village.People.Constant_Reference(Setting.Vote);
												begin
													Forms.Write_In_HTML (
														Output,
														Form,
														"処刑には" &
														Villages.Text.Name (Target) &
														"を推しています。");
												end;
											end if;
											String'Write (Output, "</div>");
										end;
									else
										Vote_Form (Output, Player_Index, Vampire.Villages.Inhabitant, 
											Special => Disallowed,
											Current => Person.Records.Constant_Reference(Village.Today).Vote,
											Current_Special => False,
											Message => "誰を処刑に……",
											Button => "投票");
									end if;
								end if;
							elsif Tag = "ability" then
								if Village.State = Playing
									and then not Person.Commited
									and then (Message_Counts(Player_Index).Speech > 0
										or else (Village.Time = Night
											and then Village.People.Constant_Reference(Player_Index).Records.Constant_Reference(Village.Today).State /= Died))
								then
									case Person.Role is
										when Vampire.Villages.Inhabitant | Vampire.Villages.Loved_Inhabitant | Vampire.Villages.Unfortunate_Inhabitant
											| Vampire.Villages.Lover | Vampire.Villages.Sweetheart_M | Vampire.Villages.Sweetheart_F
											| Vampire.Villages.Servant | Vampire.Villages.Gremlin => null;
										when Vampire.Villages.Doctor =>
											case Village.Doctor_State (Player_Index) is
												when Disallowed =>
													String'Write (Output, "<div>");
													if Village.Time = Night then
														Forms.Write_In_HTML (Output, Form, "夜は行動できません。");
													else
														Forms.Write_In_HTML (Output, Form, "今は他に犠牲者がいないと信じましょう。");
													end if;
													String'Write (Output, "</div>");
												when Allowed =>
													Vote_Form (
														Output,
														Player_Index,
														Vampire.Villages.Doctor,
														Disallowed,
														Person.Records.Constant_Reference (Village.Today).Target,
														False,
														"貴重な薬を誰に……",
														"診察");
												when Already_Used =>
													null;
											end case;
										when Vampire.Villages.Detective =>
											case Village.Detective_State (Player_Index) is
												when Disallowed =>
													String'Write (Output, "<div>");
													if Village.Time = Night then
														Forms.Write_In_HTML (Output, Form, "夜は行動できません。");
													elsif Village.Execution = Vampire.Villages.Dummy_Killed_And_From_First and then Day <= 1 then
														Forms.Write_In_HTML (Output, Form, "地主さんを調査しています。");
													else
														Forms.Write_In_HTML (Output, Form, "まだ村人に被害者はいません。");
													end if;
													String'Write (Output, "</div>");
												when Allowed =>
													Vote_Form (
														Output,
														Player_Index,
														Vampire.Villages.Detective,
														Disallowed,
														Person.Records.Constant_Reference (Village.Today).Target,
														False,
														"どの被害者を調べますか……",
														"調査");
												when Already_Used =>
													null;
											end case;
										when Vampire.Villages.Astronomer =>
											Vote_Form (
												Output,
												Player_Index,
												Astronomer,
												Disallowed,
												Person.Records.Constant_Reference (Village.Astronomer_Target_Day).Target,
												False,
												"どの家の上空の星が奇麗……",
												"観測");
										when Vampire.Villages.Hunter =>
											Vote_Form (
												Output,
												Player_Index,
												Vampire.Villages.Hunter,
												Village.Silver_Bullet_State (Player_Index),
												Person.Records.Constant_Reference (Village.Target_Day).Target,
												Person.Records.Constant_Reference (Village.Target_Day).Special,
												"誰を守りますか……",
												"護衛");
										when Vampire.Villages.Vampire_Role =>
											Vote_Form (
												Output,
												Player_Index,
												Vampire.Villages.Vampire_K,
												Disallowed,
												Person.Records.Constant_Reference (Village.Target_Day).Target,
												False,
												"誰の血が旨そうでしょうか……",
												"襲撃");
									end case;
								end if;
							elsif Tag = "action" then
								if Village.State = Playing and then (
									Message_Counts (Player_Index).Wake = 0
									or else Message_Counts (Player_Index).Encourage = 0
									or else ((Village.People.Constant_Reference (Player_Index).Role in Vampire.Villages.Vampire_Role)
										and then (
											Message_Counts (Player_Index).Vampire_Gaze = 0
											or else Message_Counts (Player_Index).Vampire_Cancel = 0)))
								then
									if Form.Template_Set = Forms.For_Full then
										String'Write (Output, "<form method=""POST"" class=""inner"">" & Line_Break);
									else
										String'Write (Output, "<form method=""POST"" action=");
										Forms.Write_Link (
											Output,
											Form,
											Current_Directory => Current_Directory,
											Resource => Forms.Self,
											Parameters => Form.Parameters_To_Village_Page (
												Village_Id => Village_Id,
												User_Id => User_Id,
												User_Password => User_Password));
										Character'Write (Output, '>');
									end if;
									String'Write (Output, "<select name=""target"">" & Line_Break);
									String'Write (Output, "<option value=""-1"" selected=""selected""></option>" & Line_Break);
									for I in Village.People.First_Index .. Village.People.Last_Index loop
										if I /= Player_Index
											and then Village.People.Constant_Reference (I).Records.Constant_Reference (Village.Today).State /= Vampire.Villages.Died
										then
											declare
												Person : Vampire.Villages.Person_Type renames Village.People.Constant_Reference (I);
											begin
												String'Write (Output, "<option value=""" & Image (I) & """>");
												Forms.Write_In_HTML (Output, Form, Villages.Text.Name (Person));
												String'Write (Output, "</option>");
											end;
										end if;
									end loop;
									String'Write(Output, "</select>" & Line_Break);
									String'Write(Output, "<select name=""action"">" & Line_Break);
									String'Write(Output, "<option value="""" selected=""selected""></option>" & Line_Break);
									if Message_Counts(Player_Index).Wake = 0 then
										String'Write(Output, "<option value=""wake"">");
										Forms.Write_In_HTML (Output, Form, "を起こす");
										String'Write(Output, "</option>" & Line_Break);
									end if;
									if Message_Counts(Player_Index).Encourage = 0 then
										String'Write(Output, "<option value=""encourage"">");
										Forms.Write_In_HTML (Output, Form, "に話の続きを促す");
										String'Write (Output, "</option>" & Line_Break);
									end if;
									if Village.Vampire_Action_Set /= None
										and then Village.People.Constant_Reference(Player_Index).Role in Vampire.Villages.Vampire_Role
										and then Message_Counts(Player_Index).Vampire_Gaze = 0
									then
										String'Write(Output, "<option value=""vampire_gaze"">");
										Forms.Write_In_HTML (Output, Form, "をこっそり見つめる。");
										String'Write(Output, "</option>" & Line_Break);
									end if;
									if Village.Vampire_Action_Set = Gaze_And_Cancel
										and then Village.People.Constant_Reference(Player_Index).Role in Vampire.Villages.Vampire_Role
										and then Message_Counts(Player_Index).Vampire_Cancel = 0
									then
										String'Write(Output, "<option value=""vampire_cancel"">");
										Forms.Write_In_HTML (Output, Form, "を襲うのをやめさせる。");
										String'Write(Output, "</option>" & Line_Break);
									end if;
									String'Write(Output, "</select>" & Line_Break);
									String'Write(Output, "<input type=""submit"" ");
									Forms.Write_Attribute_Name (Output, "value");
									Forms.Write_Attribute_Open (Output);
									Forms.Write_In_Attribute (Output, Form, "行動");
									Forms.Write_Attribute_Close (Output);
									String'Write (Output, "/>");
									String'Write(Output, "<input type=""hidden"" name=""cmd"" value=""action"" />" & Line_Break &
										"</form>" & Line_Break);
								end if;
							elsif Tag = "active" then
								if not Person.Commited
									and then Village.State /= Epilogue
									and then (Message_Counts (Player_Index).Speech > 0 or else Village.State = Prologue)
								then
									declare
										Setting : Vampire.Villages.Person_Record
											renames Person.Records.Constant_Reference (Village.Today);
									begin
										if Setting.State /= Vampire.Villages.Died then
											Web.Producers.Produce (Output, Template, Handler => Handle_Player'Access);
										end if;
									end;
								end if;
							elsif Tag = "escape" then
								if Village.State = Prologue then
									declare
										subtype Arg is Integer range 1000 .. 4999;
										function Random_Arg is
											new Ada.Numerics.Distributions.Linear_Discrete_Random (
												Ada.Numerics.MT19937.Cardinal,
												Arg,
												Ada.Numerics.MT19937.Generator,
												Ada.Numerics.MT19937.Random_32);
										X : Arg;
										Y : Arg;
										procedure Handle_Escape (
											Output : not null access Ada.Streams.Root_Stream_Type'Class;
											Tag : in String;
											Template : in Web.Producers.Template) is
										begin
											if Tag = "x" then
												Forms.Write_In_HTML (Output, Form, Image (X));
											elsif Tag = "value_x" then
												Forms.Write_Attribute_Name (Output, "value");
												Forms.Write_Attribute_Open (Output);
												Forms.Write_In_Attribute (Output, Form, Image (X));
												Forms.Write_Attribute_Close (Output);
											elsif Tag = "y" then
												Forms.Write_In_HTML (Output, Form, Image (Y));
											elsif Tag = "value_y" then
												Forms.Write_Attribute_Name (Output, "value");
												Forms.Write_Attribute_Open (Output);
												Forms.Write_In_Attribute (Output, Form, Image (Y));
												Forms.Write_Attribute_Close (Output);
											elsif Tag = "action_page" then
												Forms.Write_Attribute_Name (Output, "action");
												Forms.Write_Link (
													Output,
													Form,
													Current_Directory => Current_Directory,
													Resource => Forms.Self,
													Parameters => Form.Parameters_To_Village_Page (
														Village_Id => Village_Id,
														User_Id => User_Id,
														User_Password => User_Password));
											else
												raise Program_Error with "Invalid template """ & Tag & """";
											end if;
										end Handle_Escape;
										Seed : aliased Ada.Numerics.MT19937.Generator :=
											Ada.Numerics.MT19937.Initialize;
									begin
										X := Random_Arg (Seed);
										Y := Random_Arg (Seed);
										Web.Producers.Produce (Output, Template, Handler => Handle_Escape'Access);
									end;
								end if;
							elsif Tag = "commited" then
								if Person.Commited then
									Web.Producers.Produce (Output, Template, Handler => Handle_Player'Access);
								end if;
							elsif Tag = "action_page" then
								Forms.Write_Attribute_Name (Output, "action");
								Forms.Write_Link (
									Output,
									Form,
									Current_Directory => Current_Directory,
									Resource => Forms.Self,
									Parameters => Form.Parameters_To_Village_Page (
										Village_Id => Village_Id,
										User_Id => User_Id,
										User_Password => User_Password));
							else
								raise Program_Error with "Invalid template """ & Tag & """";
							end if;
						end Handle_Player;
					begin
						Web.Producers.Produce (Output, Template, "player", Handler => Handle_Player'Access);
					end;
				elsif User_Id = Tabula.Users.Administrator then
					Web.Producers.Produce (Output, Template, "administrator", Handler=> Handle'Access);
				elsif Village.State > Prologue then
					Web.Producers.Produce (Output, Template, "opened");
				elsif Village.People.Length >= Maximum_Number_Of_Persons then
					Web.Producers.Produce (Output, Template, "over");
				else
					declare
						Cast : Casts.Cast_Collection := Casts.Load (Cast_File_Name);
						procedure Handle_Entry (
							Output : not null access Ada.Streams.Root_Stream_Type'Class;
							Tag : in String;
							Template : in Web.Producers.Template) is
						begin
							if Tag = "works" then
								String'Write (Output, "<option value=""-1"" selected=""selected"">");
								Forms.Write_In_HTML (Output, Form, "(既定)");
								String'Write (Output, "</option>");
								for Position in Cast.Works.First_Index .. Cast.Works.Last_Index loop
									declare
										Item : Casts.Work renames Cast.Works.Constant_Reference (Position);
									begin
										if not Casts.Is_Empty (Item) then
											String'Write (Output, "<option ");
											Forms.Write_Attribute_Name (Output, "value");
											Forms.Write_Attribute_Open (Output);
											Forms.Write_In_Attribute (Output, Form, Image (Position));
											Forms.Write_Attribute_Close (Output);
											Character'Write (Output, '>');
											Forms.Write_In_HTML (Output, Form, Item.Name.Constant_Reference);
											case Item.Sex is
												when Casts.Male =>
													Forms.Write_In_HTML (Output, Form, " (男性職)");
												when Casts.Female =>
													Forms.Write_In_HTML (Output, Form, " (女性職)");
												when Casts.Neutral =>
													null;
											end case;
											if Item.Nominated then
												Forms.Write_In_HTML (Output, Form, " (指名職)");
											end if;
											String'Write (Output, "</option>");
										end if;
									end;
								end loop;
							elsif Tag = "names" then
								declare
									type Sex_To_String is array (Casts.Person_Sex) of String (1 .. 9);
									Sex_Name : constant Sex_To_String := (" (男性)", " (女性)");
								begin
									for Position in Cast.People.First_Index .. Cast.People.Last_Index loop
										declare
											Item : Casts.Person renames Cast.People.Constant_Reference (Position);
										begin
											if not Casts.Is_Empty (Item) and then Item.Group = Village.Face_Group then
												String'Write (Output, "<option ");
												Forms.Write_Attribute_Name (Output, "value");
												Forms.Write_Attribute_Open (Output);
												Forms.Write_In_Attribute (Output, Form, Image (Position));
												Forms.Write_Attribute_Close (Output);
												Character'Write (Output, '>');
												Forms.Write_In_HTML (Output, Form,
													Item.Name.Constant_Reference &
													Sex_Name (Item.Sex));
												String'Write (Output, "</option>");
											end if;
										end;
									end loop;
								end;
							elsif Tag = "request" then
								for I in Vampire.Villages.Requested_Role loop
									String'Write (Output, "<option ");
									Forms.Write_Attribute_Name (Output, "value");
									Forms.Write_Attribute_Open (Output);
									Forms.Write_In_Attribute (Output, Form, Vampire.Villages.Requested_Role'Image (I));
									Forms.Write_Attribute_Close (Output);
									Character'Write (Output, '>');
									Forms.Write_In_HTML (Output, Form, Villages.Text.Image (I));
									String'Write (Output, "</option>");
								end loop;
							elsif Tag = "groups" then
								for I in Cast.Groups.First_Index .. Cast.Groups.Last_Index loop
									declare
										Item : Casts.Group renames Cast.Groups.Constant_Reference (I);
									begin
										String'Write (Output, "<option ");
										Forms.Write_Attribute_Name (Output, "value");
										Forms.Write_Attribute_Open (Output);
										Forms.Write_In_Attribute (Output, Form, Image (Item.Group));
										Forms.Write_Attribute_Close (Output);
										if Item.Group = Village.Face_Group then
											Forms.Write_Attribute_Name (Output, "selected");
											Forms.Write_Attribute_Open (Output);
											Forms.Write_In_Attribute (Output, Form, "selected");
											Forms.Write_Attribute_Close (Output);
										end if;
										Character'Write (Output, '>');
										Forms.Write_In_HTML (Output, Form, Item.Name.Constant_Reference);
										if Item.Group = Village.Face_Group then
											Forms.Write_In_HTML (Output, Form, " *");
										end if;
										String'Write (Output, "</option>");
									end;
								end loop;
							elsif Tag = "facegroups" then
								if Village.People.Is_Empty then
									Web.Producers.Produce (Output, Template, Handler => Handle_Entry'Access);
								end if;
							elsif Tag = "action_page" then
								Forms.Write_Attribute_Name (Output, "action");
								Forms.Write_Link (
									Output,
									Form,
									Current_Directory => Current_Directory,
									Resource => Forms.Self,
									Parameters => Form.Parameters_To_Village_Page (
										Village_Id => Village_Id,
										User_Id => User_Id,
										User_Password => User_Password));
							else
								raise Program_Error with "Invalid template """ & Tag & """";
							end if;
						end Handle_Entry;
					begin
						Vampire.Villages.Exclude_Taken (Cast, Village);
						Web.Producers.Produce (Output, Template, "entry", Handler => Handle_Entry'Access);
					end;
				end if;
			end if;
		elsif Tag = "next" then
			if Day < Village.Today and then Tip_Showed then
				declare
					procedure Handle_Next (
						Output : not null access Ada.Streams.Root_Stream_Type'Class;
						Tag : in String;
						Template : in Web.Producers.Template) is
					begin
						if Tag = "href_next" then
							Forms.Write_Attribute_Name (Output, "href");
							Forms.Write_Link_To_Village_Page (
								Output,
								Form,
								Current_Directory => Current_Directory,
								HTML_Directory => HTML_Directory,
								Log => Log,
								Village_Id => Village_Id,
								Day => Day + 1,
								User_Id => User_Id,
								User_Password => User_Password);
						else
							raise Program_Error with "Invalid template """ & Tag & """";
						end if;
					end Handle_Next;
				begin
					Web.Producers.Produce (Output, Template, Handler => Handle_Next'Access);
				end;
			end if;
		elsif Tag = "scroll" then
			if Village.State /= Closed
				and then Day = Village.Today
				and then Player_Index >= 0
			then
				Web.Producers.Produce (Output, Template);
			end if;
		else
			raise Program_Error with "Invalid template """ & Tag & """";
		end if;
	end Handle;
begin
	Web.Producers.Produce (Output, Template, Handler => Handle'Access);
end Vampire.R3.Village_Page;

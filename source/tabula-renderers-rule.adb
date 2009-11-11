-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Unchecked_Conversion;
package body Tabula.Renderers.Rule is
	use type Villages.Servant_Knowing_Mode;
	use type Villages.Monster_Side;
	use type Villages.Attack_Mode;
	use type Villages.Teaming;
	use type Villages.Hunter_Silver_Bullet_Mode;
	use type Villages.Unfortunate_Mode;
	use type Villages.Village_State;
	use type Villages.Daytime_Preview_Mode;
	use type Villages.Doctor_Infected_Mode;
	
	type String_Access is not null access constant String;
	type Item is record
		Value : String_Access;
		Guide : String_Access;
		Unrecommended : Boolean := False;
	end record;
	type Item_Array is array(Natural range <>) of Item;
	
	Victim_Existing_Message : constant Item_Array(Boolean'Pos(False) .. Boolean'Pos(True)) := (
		Boolean'Pos(False) => (
			Value => new String'(Boolean'Image(False)),
			Guide => new String'("能力者が揃った状態で開始します。"),
			others => <>),
		Boolean'Pos(True) => (
			Value => new String'(Boolean'Image(True)),
			Guide => new String'("能力者が死亡済みの可能性があります。"),
			others => <>));
	
	First_Execution_Message : constant Item_Array(Boolean'Pos(False) .. Boolean'Pos(True)) := (
		Boolean'Pos(False) => (
			Value => new String'(Boolean'Image(False)),
			Guide => new String'("初日は処刑を行いません。"),
			others => <>),
		Boolean'Pos(True) => (
			Value => new String'(Boolean'Image(True)),
			Guide => new String'("初日から処刑を行います。"),
			others => <>));
	
	subtype VT is Villages.Teaming;
	Teaming_Message : constant Item_Array(VT'Pos(VT'First) .. VT'Pos(VT'Last)) := (
		VT'Pos(Villages.Low_Density) => (
			Value => new String'(VT'Image(Villages.Low_Density)),
			Guide => new String'("能力者の密度を線形にします。"),
			others => <>),
		VT'Pos(Villages.Shuffling_Headless) => (
			Value => new String'(VT'Image(Villages.Shuffling_Headless)),
			Guide => new String'("村側能力者を増やします(首無し騎士に似せます)。"),
			Unrecommended => True),
		VT'Pos(Villages.Shuffling_Euro) => (
			Value => new String'(Villages.Teaming'Image(Villages.Shuffling_Euro)),
			Guide => new String'("妖魔が早く出ます(欧州に似せます)。"),
			others => <>),
		VT'Pos(Villages.Shuffling) => (
			Value => new String'(Villages.Teaming'Image(Villages.Shuffling)),
			Guide => new String'("基本的な編成です。"),
			others => <>), 
		VT'Pos(Villages.Shuffling_Gremlin) => (
			Value => new String'(Villages.Teaming'Image(Villages.Shuffling_Gremlin)),
			Guide => new String'("基本的な編成に加え、妖魔が早く出ます。"),
			others => <>), 
		VT'Pos(Villages.Hiding) => (
			Value => new String'(Villages.Teaming'Image(Villages.Hiding)),
			Guide => new String'("村側能力者の構成はわかりません。"),
			others => <>),
		VT'Pos(Villages.Hiding_Gremlin) => (
			Value => new String'(Villages.Teaming'Image(Villages.Hiding_Gremlin)),
			Guide => new String'("村側能力者の構成はわからず、妖魔が早く出ます。"),
			others => <>));
	
	subtype MS is Villages.Monster_Side;
	Monster_Side_Message : constant Item_Array(MS'Pos(MS'First) .. MS'Pos(MS'Last)) := (
		MS'Pos(Villages.Fixed) => (
			Value => new String'(Villages.Monster_Side'Image(Villages.Fixed)),
			Guide => new String'("吸血鬼が襲ってきます。"),
			others => <>), 
		MS'Pos(Villages.Shuffling) => (
			Value => new String'(Villages.Monster_Side'Image(Villages.Shuffling)),
			Guide => new String'("吸血鬼の全貌はわかりません。"),
			Unrecommended => True),
		MS'Pos(Villages.Gremlin) => (
			Value => new String'(Villages.Monster_Side'Image(Villages.Gremlin)),
			Guide => new String'("使徒よりも妖魔が先に現れます。"),
			others => <>));
	
	subtype AM is Villages.Attack_Mode;
	Attack_Message : constant Item_Array(AM'Pos(AM'First) .. AM'Pos(AM'Last)) := (
		AM'Pos(Villages.Two) => (
			Value => new String'(AM'Image(Villages.Two)), 
			Guide => new String'("吸血鬼ふたり以上に襲われると死亡します。"),
			others => <>),
		AM'Pos(Villages.Mocturnal_Infecting) => (
			Value => new String'(AM'Image(Villages.Mocturnal_Infecting)), 
			Guide => new String'("天文家と猟師は感染したら襲撃を行います。"),
			others => <>),
		AM'Pos(Villages.Unanimity) => (
			Value => new String'(AM'Image(Villages.Unanimity)), 
			Guide => new String'("すべての吸血鬼に襲われると死亡します。"),
			others => <>));
	
	subtype SK is Villages.Servant_Knowing_Mode;
	Servant_Knowing_Message : constant Item_Array(SK'Pos(SK'First) .. SK'Pos(SK'Last)) := (
		SK'Pos(Villages.None) => (
			Value => new String'(SK'Image(Villages.None)),
			Guide => new String'("使徒は吸血鬼の正体を知りません。"),
			others => <>),
		SK'Pos(Villages.Vampire_K) => (
			Value => new String'(SK'Image(Villages.Vampire_K)),
			Guide => new String'("使徒は吸血鬼の王を知っています。"),
			others => <>),
		SK'Pos(Villages.Vampires) => (
			Value => new String'(SK'Image(Villages.Vampires)),
			Guide => new String'("使徒は吸血鬼を知っています。"),
			others => <>));
	
	subtype DP is Villages.Daytime_Preview_Mode;
	Daytime_Preview_Message : constant Item_Array(DP'Pos(DP'First) .. DP'Pos(DP'Last)) := (
		DP'Pos(Villages.None) => (
			Value => new String'(DP'Image(Villages.None)),
			Guide => new String'("探偵と医者は翌日まで結果がわかりません。"),
			Unrecommended => True),
		DP'Pos(Villages.Role_Only) => (
			Value => new String'(DP'Image(Villages.Role_Only)),
			Guide => new String'("探偵は日中に正体を調べ翌日までに遺言を調べます。"),
			others => <>),
		DP'Pos(Villages.Message_Only) => (
			Value => new String'(DP'Image(Villages.Message_Only)),
			Guide => new String'("探偵は日中に遺言を調べ翌日までに正体を調べます。"),
			others => <>),
		DP'Pos(Villages.Role_And_Message) => (
			Value => new String'(DP'Image(Villages.Role_And_Message)),
			Guide => new String'("探偵は日中に正体と遺言を調べます。"),
			others => <>));

	subtype DI is Villages.Doctor_Infected_Mode;
	Doctor_Infected_Message : constant Item_Array(DI'Pos(DI'First) .. DI'Pos(DI'Last)) := (
		DI'Pos(Villages.Cure) => (
			Value => new String'(DI'Image(Villages.Cure)),
			Guide => new String'("医者自身の感染は治療に影響しません。"),
			others => <>),
		DI'Pos(Villages.Find_Infection) => (
			Value => new String'(DI'Image(Villages.Find_Infection)),
			Guide => new String'("医者自身が感染していると治療の効果はありません。"),
			others => <>));

	subtype HS is Villages.Hunter_Silver_Bullet_Mode;
	Hunter_Silver_Bullet_Message : constant Item_Array(HS'Pos(HS'First) .. HS'Pos(HS'Last)) := (
		HS'Pos(Villages.Target) => (
			Value => new String'(HS'Image(Villages.Target)),
			Guide => new String'("護衛対象が襲われたとき銀の弾丸は吸血鬼を殺します。"),
			others => <>),
		HS'Pos(Villages.Target_And_Self) => (
			Value => new String'(HS'Image(Villages.Target_And_Self)),
			Guide => new String'("護衛対象または猟師が襲われたとき銀の弾丸は吸血鬼を殺します。"),
			others => <>));

	subtype UM is Villages.Unfortunate_Mode;
	Unfortunate_Message : constant Item_Array(UM'Pos(UM'First) .. UM'Pos(UM'Last)) := (
		UM'Pos(Villages.None) => (
			Value => new String'(UM'Image(Villages.None)),
			Guide => new String'("数奇な運命の村人はいません。"),
			others => <>),
		UM'Pos(Villages.Appear) => (
			Value => new String'(UM'Image(Villages.Appear)),
			Guide => new String'("数奇な運命の村人がいるかもしれません。"),
			Unrecommended => True),
		UM'Pos(Villages.Infected_Only) => (
			Value => new String'(UM'Image(Villages.Infected_Only)),
			Guide => new String'("数奇な運命の村人は襲撃では殺されません。"),
			others => <>));
	
	Day_Duration_Message : constant Item_Array(1 .. 4) := (
		1 => (
			Value => new String'(Duration'Image(15 * 60.0)),
			Guide => new String'("ゲームの1日は実時間の15分です。"),
			others => <>),
		2 => (
			Value => new String'(Duration'Image(20 * 60.0)),
			Guide => new String'("ゲームの1日は実時間の20分です。"),
			others => <>),
		3 => (
			Value => new String'(Duration'Image(25 * 60.0)),
			Guide => new String'("ゲームの1日は実時間の25分です。"),
			others => <>), 
		4 => (
			Value => new String'(Duration'Image(30 * 60.0)),
			Guide => new String'("ゲームの1日は実時間の30分です。"),
			others => <>));

	Night_Duration_Message : constant Item_Array(1 .. 2) := (
		1 => (
			Value => new String'(Duration'Image(0.0)),
			Guide => new String'("夜はありません。"),
			others => <>),
		2 => (
			Value => new String'(Duration'Image(5 * 60.0)),
			Guide => new String'("夜は5分です。"),
			others => <>));
	
	procedure List(
		Object : in Renderer'Class;
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Name : String;
		Items : Item_Array;
		Selected : Integer := -1) is
	begin
		Write(Output, "<select name=""");
		Write(Output, Name);
		Write(Output, """>");
		for I in Items'Range loop
			Write(Output, "<option value=""");
			Write(Output, Items(I).Value.all);
			Write(Output, '"');
			if I = Selected then
				Write(Output, " selected=""selected""");
			end if;
			Write(Output, '>');
			Web.Write_In_HTML (Output, Object.HTML_Version, Items(I).Guide.all);
			if Items(I).Unrecommended then
				Write(Output, " お薦めしません。");
			end if;
			if I = Selected then
				Write(Output, " *");
			end if;
			Write(Output, "</option>");
		end loop;
		Write(Output, "</select>");
	end List;
	
	procedure Rule_Panel(
		Object : in Renderer'Class;
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Template : in System.Address; -- Web.Producers.Template;
		Village_Id : in Villages.Lists.Village_Id;
		Village : in Villages.Village_Type; 
		Player : in Boolean;
		User_Id : in String;
		User_Password : in String)
	is
		type Template_Access is access constant Web.Producers.Template;
		function "+" is new Ada.Unchecked_Conversion(System.Address, Template_Access);
		Template_Body : Template_Access := + Template;
		Changable: Boolean;
		procedure Handle(Output : not null access Ada.Streams.Root_Stream_Type'Class;
			Tag : in String; Template : in Web.Producers.Template) is
		begin
			if Tag = "items" then
				if Changable then
					if Village.Day_Duration < 24 * 60 * 60.0 then
						for I in Day_Duration_Message'Range loop
							if Duration'Value(Day_Duration_Message(I).Value.all) = Village.Day_Duration then
								List(Object, Output,
									Name => "day-duration", 
									Items => Day_Duration_Message,
									Selected => I);
								Web.Producers.Produce(Output, Template);
								exit;
							end if;
						end loop;
						for I in Night_Duration_Message'Range loop
							if Duration'Value(Night_Duration_Message(I).Value.all) = Village.Night_Duration then
								List(Object, Output,
									Name => "night-duration", 
									Items => Night_Duration_Message,
									Selected => I);
								Web.Producers.Produce(Output, Template);
								exit;
							end if;
						end loop;
					end if;
					List(Object, Output,
						Name => "victim-existing", 
						Items => Victim_Existing_Message,
						Selected => Boolean'Pos(Village.Victim_Existing));
					Web.Producers.Produce(Output, Template);
					List(Object, Output,
						Name => "first-execution", 
						Items => First_Execution_Message,
						Selected => Boolean'Pos(Village.First_Execution));
					Web.Producers.Produce(Output, Template);
					List(Object, Output,
						Name => "teaming", 
						Items => Teaming_Message,
						Selected => Villages.Teaming'Pos(Village.Teaming));
					Web.Producers.Produce(Output, Template);
					List(Object, Output,
						Name => "monster-side", 
						Items => Monster_Side_Message,
						Selected => Villages.Monster_Side'Pos(Village.Monster_Side));
					Web.Producers.Produce(Output, Template);
					List(Object, Output,
						Name => "attack", 
						Items => Attack_Message,
						Selected => Villages.Attack_Mode'Pos(Village.Attack));
					Web.Producers.Produce(Output, Template);
					List(Object, Output,
						Name => "servant-knowing", 
						Items => Servant_Knowing_Message,
						Selected => Villages.Servant_Knowing_Mode'Pos(Village.Servant_Knowing));
					Web.Producers.Produce(Output, Template);
					List(Object, Output,
						Name => "daytime-preview", 
						Items => Daytime_Preview_Message,
						Selected => Villages.Daytime_Preview_Mode'Pos(Village.Daytime_Preview));
					Web.Producers.Produce(Output, Template);
					List(Object, Output,
						Name => "doctor-infected", 
						Items => Doctor_Infected_Message,
						Selected => Villages.Doctor_Infected_Mode'Pos(Village.Doctor_Infected));
					Web.Producers.Produce(Output, Template);
					List(Object, Output,
						Name => "hunter-silver-bullet", 
						Items => Hunter_Silver_Bullet_Message,
						Selected => Villages.Hunter_Silver_Bullet_Mode'Pos(Village.Hunter_Silver_Bullet));
					Web.Producers.Produce(Output, Template);
					List(Object, Output,
						Name => "unfortunate", 
						Items => Unfortunate_Message,
						Selected => Villages.Unfortunate_Mode'Pos(Village.Unfortunate));
				else
					if Village.Day_Duration < 24 * 60 * 60.0 then
						for I in Day_Duration_Message'Range loop
							if Duration'Value(Day_Duration_Message(I).Value.all) = Village.Day_Duration then
								Write(Output, Day_Duration_Message(I).Guide.all);
								Write(Output, ' ' & Ascii.LF);
								exit;
							end if;
						end loop;
						for I in Night_Duration_Message'Range loop
							if Duration'Value(Night_Duration_Message(I).Value.all) = Village.Night_Duration then
								Write(Output, Night_Duration_Message(I).Guide.all);
								Write(Output, ' ' & Ascii.LF);
								exit;
							end if;
						end loop;
					end if;
					declare
						procedure Put(Items : Item_Array; Index : Natural) is
						begin
							Write(Output, Items(Index).Guide.all);
							if Village.State /= Villages.Closed and then Items(Index).Unrecommended then
								Write(Output, " <em>お薦めしません。</em>");
							end if;
							Write(Output, ' ' & Ascii.LF);
						end Put;
					begin
						if Player or else Village.Victim_Existing /= Villages.Initial_Victim_Existing then
							Put(Victim_Existing_Message, Boolean'Pos(Village.Victim_Existing));
						end if;
						if Player or else Village.First_Execution /= Villages.Initial_First_Execution then
							Put (First_Execution_Message, Boolean'Pos (Village.First_Execution));
						end if;
						if Player or else Village.Teaming /= Villages.Initial_Teaming then
							Put(Teaming_Message, Villages.Teaming'Pos(Village.Teaming));
						end if;
						if Player or else Village.Monster_Side /= Villages.Initial_Monster_Side then
							Put(Monster_Side_Message, Villages.Monster_SIde'Pos(Village.Monster_Side));
						end if;
						if Player or else Village.Attack /= Villages.Initial_Attack then
							Put(Attack_Message, Villages.Attack_Mode'Pos(Village.Attack));
						end if;
						if Player or else Village.Servant_Knowing /= Villages.Initial_Servant_Knowing then
							Put(Servant_Knowing_Message, Villages.Servant_Knowing_Mode'Pos(Village.Servant_Knowing));
						end if;
						if Player or else Village.Daytime_Preview /= Villages.Initial_Daytime_Preview then
							Put(Daytime_Preview_Message, Villages.Daytime_Preview_Mode'Pos(Village.Daytime_Preview));
						end if;
						if Player or else Village.Doctor_Infected /= Villages.Initial_Doctor_Infected then
							Put(Doctor_Infected_Message, Villages.Doctor_Infected_Mode'Pos(Village.Doctor_Infected));
						end if;
						if Player or else Village.Hunter_Silver_Bullet /= Villages.Initial_Hunter_Silver_Bullet then
							Put(Hunter_Silver_Bullet_Message, Villages.Hunter_Silver_Bullet_Mode'Pos(Village.Hunter_Silver_Bullet));
						end if;
						if Player or else Village.Unfortunate /= Villages.Initial_Unfortunate then
							Put(Unfortunate_Message, Villages.Unfortunate_Mode'Pos(Village.Unfortunate));
						end if;
					end;
				end if;
			elsif Tag = "uri" then
				Link(Object, Output, Village_Id => Village_Id,
					User_Id => User_Id, User_Password => User_Password);
			else
				raise Program_Error with "Invalid template """ & Tag & """";
			end if;
		end Handle;
		Extract : constant array(Boolean) of access constant String := (
			new String'("static"), new String'("changable"));
	begin
		Changable := False;
		if Player and Village.Today = 0 then
			Changable := True;
			for I in Village.People.First_Index .. Village.People.Last_Index loop
				if Village.People.Constant_Reference(I).Element.Commited then
					Changable := False;
				end if;
			end loop;
		end if;
		if Player 
			or else Village.Day_Duration < 24 * 60 * 60.0
			or else Village.Victim_Existing      /= Villages.Initial_Victim_Existing
			or else Village.First_Execution      /= Villages.Initial_First_Execution
			or else Village.Teaming              /= Villages.Initial_Teaming
			or else Village.Monster_Side         /= Villages.Initial_Monster_Side
			or else Village.Attack               /= Villages.Initial_Attack
			or else Village.Servant_Knowing      /= Villages.Initial_Servant_Knowing
			or else Village.Hunter_Silver_Bullet /= Villages.Initial_Hunter_Silver_Bullet
			or else Village.Unfortunate          /= Villages.Initial_Unfortunate
		then
			Web.Producers.Produce(Output, Template_Body.all, Extract(Changable).all, Handler => Handle'Access);
		end if;
	end Rule_Panel;
	
	procedure Change(Village : in out Villages.Village_Type; Inputs : in Web.Query_Strings) is
		Day_D : String renames Web.Element(Inputs, "day-duration");
	begin
		if Day_D /= "" and then Village.Day_Duration < 24 * 60 * 60.0 then
			Village.Day_Duration := Duration'Value(Day_D);
			Village.Night_Duration := Duration'Value(Web.Element(Inputs, "night-duration"));
		end if;
		Village.Victim_Existing := Boolean'Value(Web.Element(Inputs, "victim-existing"));
		Village.First_Execution := Boolean'Value(Web.Element(Inputs, "first-execution"));
		Village.Teaming := Villages.Teaming'Value(Web.Element(Inputs, "teaming"));
		Village.Attack := Villages.Attack_Mode'Value(Web.Element(Inputs, "attack"));
		Village.Monster_Side := Villages.Monster_Side'Value(Web.Element(Inputs, "monster-side"));
		Village.Servant_Knowing := Villages.Servant_Knowing_Mode'Value(Web.Element(Inputs, "servant-knowing"));
		Village.Daytime_Preview := Villages.Daytime_Preview_Mode'Value(Web.Element(Inputs, "daytime-preview"));
		Village.Doctor_Infected := Villages.Doctor_Infected_Mode'Value(Web.Element(Inputs, "doctor-infected"));
		Village.Hunter_Silver_Bullet := Villages.Hunter_Silver_Bullet_Mode'Value(Web.Element(Inputs, "hunter-silver-bullet"));
		Village.Unfortunate := Villages.Unfortunate_Mode'Value(Web.Element(Inputs, "unfortunate"));
	end Change;
	
end Tabula.Renderers.Rule;

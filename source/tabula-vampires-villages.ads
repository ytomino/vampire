-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Calendar;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Tabula.Calendar;
with Tabula.Casts;
with Tabula.Villages;
package Tabula.Vampires.Villages is
	
	type Execution_Mode is (Dummy_Killed_And_From_First, From_First, From_Second, Provisional_Voting_From_Second);
	type Teaming is (Low_Density, Shuffling_Headless, Shuffling_Euro, Shuffling, Shuffling_Gremlin, Hiding, Hiding_Gremlin);
	type Attack_Mode is (Two, Mocturnal_Infecting, Unanimity);
	type Servant_Knowing_Mode is (None, Vampire_K, All_Vampires);
	type Monster_Side is (Fixed, Shuffling, Gremlin);
	type Daytime_Preview_Mode is (None, Role_Only, Message_Only, Role_And_Message);
	type Doctor_Infected_Mode is (Cure, Find_Infection);
	type Hunter_Silver_Bullet_Mode is (Target, Target_And_Self);
	type Unfortunate_Mode is (None, Appear, Infected_Only);
	
	subtype Hidings is Teaming range Hiding .. Hiding_Gremlin;
	
	-- オプションルール初期値
	Initial_Execution            : constant Execution_Mode            := From_Second;
	Initial_Teaming              : constant Teaming                   := Shuffling;
	Initial_Monster_Side         : constant Monster_Side              := Fixed;
	Initial_Attack               : constant Attack_Mode               := Mocturnal_Infecting;
	Initial_Servant_Knowing      : constant Servant_Knowing_Mode      := Vampire_K;
	Initial_Daytime_Preview      : constant Daytime_Preview_Mode      := Message_Only;
	Initial_Doctor_Infected      : constant Doctor_Infected_Mode      := Find_Infection;
	Initial_Hunter_Silver_Bullet : constant Hunter_Silver_Bullet_Mode := Target_And_Self;
	Initial_Unfortunate          : constant Unfortunate_Mode          := Infected_Only;

	type Requested_Role is (Random, Rest, 
		Inhabitant, Detective, Astronomer, Doctor, Hunter, Sweetheart, Servant, Vampire, 
		Village_Side, Vampire_Side, Gremlin);
	subtype Requested_Role_No_Random is Requested_Role range Inhabitant .. Vampire;
	
	type Person_Role is (Inhabitant, Gremlin,
		Vampire_K, Vampire_Q, Vampire_J, Servant, 
		Werewolf, Possessed,
		Detective, Doctor, Astronomer, Hunter, 
		Unfortunate_Inhabitant,
		Lover, Loved_Inhabitant, Sweetheart_M, Sweetheart_F);
	subtype Matrix_Role is Person_Role range Detective .. Hunter;
	subtype Night_Role is Person_Role range Astronomer .. Hunter;
	subtype Daytime_Role is Person_Role range Detective .. Doctor;
	subtype Vampire_Role is Person_Role range Vampire_K .. Vampire_J;
	
	type Role_Appearance is (None, Random, Force);
	type Role_Appearances is array(Detective .. Lover) of Role_Appearance;
	
	type Person_State is (Normal, Infected, Died);
	
	type Person_Record is record
		State : Person_State;
		Vote : Integer;
		Provisional_Vote : Integer; -- 仮投票
		Applied : Boolean; -- 開票申請
		Candidate : Boolean; -- 投票の候補
		Target : Integer;
		Special : Boolean;
		Note : Ada.Strings.Unbounded.Unbounded_String;
	end record;
	
	Default_Person_Record : constant Person_Record := (
		State => Normal,
		Vote => -1,
		Provisional_Vote => -1,
		Applied => False,
		Candidate => True,
		Target => -1,
		Special => False,
		Note => Ada.Strings.Unbounded.Null_Unbounded_String);
	
	package Person_Records is new Ada.Containers.Vectors (Natural, Person_Record);
	
	type Person_Type is new Casts.Person with record
		Id : Ada.Strings.Unbounded.Unbounded_String;
		Request : Requested_Role;
		Ignore_Request : Boolean;
		Role : Person_Role;
		Records : aliased Person_Records.Vector;
		Commited : Boolean;
	end record;
	
	package People is new Ada.Containers.Vectors (Natural, Person_Type);
	
	type Message_Kind is (
		Narration,                        -- ト書き
		Escape,                           -- 村を出る
		Join,                             -- 参加
		Escaped_Join,                     -- 村を出た者の参加
		Speech,                           -- 通常会話
		Escaped_Speech,                   -- 村を出た者の会話
		Monologue,                        -- 独り言
		Ghost,                            -- 墓場
		Howling,                          -- 遠吠えまたは夜間の会話
		Howling_Blocked,                  -- 遠吠えまたは夜間の会話が妨害された
		Action_Wake,                      -- 起こす
		Action_Encourage,                 -- 促し
		Action_Vampire_Gaze,              -- 視線
		Action_Vampire_Gaze_Blocked,      -- 視線が妨害された
		Doctor_Cure,                      -- 治療
		Doctor_Cure_Preview,              -- 治療
		Doctor_Found_Infection,           -- 感染を発見しただけ
		Doctor_Found_Infection_Preview,   -- 感染を発見しただけ
		Doctor_Failed,                    -- 診察はしたが感染させられた患者では無かった
		Doctor_Failed_Preview,            -- 診察はしたが感染させられた患者では無かった
		Doctor_Found_Gremlin,             -- 妖魔を見つけた
		Doctor_Found_Gremlin_Preview,     -- 妖魔を見つけた
		Detective_Survey,                 -- 調査
		Detective_Survey_Preview,         -- 調査
		Detective_Survey_Victim,          -- 初日犠牲者の調査
		Provisional_Vote,                 -- 仮投票
		Execution,                        -- 処刑
		Awareness,                        -- 自覚
		Astronomer_Observation,           -- 観測
		Meeting,                          -- 吸血鬼の会話
		Vampire_Murder,                   -- 襲撃
		Vampire_Murder_And_Killed,        -- 襲撃に成功し相打ちで銀の弾丸を撃ちこまれた
		Vampire_Infection,                -- 感染
		Vampire_Infection_And_Killed,     -- 感染に成功し相打ちで銀の弾丸を撃ちこまれた
		Vampire_Failed,                   -- 襲撃に失敗
		Vampire_Failed_And_Killed,        -- 襲撃に失敗し銀の弾丸を撃ちこまれた
		Hunter_Guard,                     -- 護衛に成功した
		Hunter_Guard_With_Silver,         -- 護衛に成功し銀の弾丸を撃ちこんだ
		Hunter_Nothing_With_Silver,       -- 銀の弾丸を込めていたが何も無かった
		Hunter_Infected_With_Silver,      -- 誰かを護衛していたわけではないが自分が襲われたので銀の弾丸で反撃した
		Hunter_Killed_With_Silver,        -- 銀の弾丸で相打ち
		Hunter_Failed,                    -- ガードしたが吸血鬼は来なかった
		Hunter_Failed_With_Silver,        -- ガードしたが吸血鬼は来ず銀の弾丸を無駄遣いした
		Gremlin_Sense,                    -- 妖魔が吸血鬼の残数を知る
		Gremlin_Killed,                   -- 妖魔が死んだ
		Sweetheart_Incongruity,           -- 違和感
		Sweetheart_Suicide,               -- 後追い
		Servant_Knew_Vampire_K,           -- Kを知る
		Servant_Knew_Vampires,            -- 吸血鬼全員を知る
		List,                             -- 一覧
		Introduction,                     -- 序文
		Breakdown);                       -- 開始
	
	subtype Action_Message_Kind is Message_Kind range Action_Wake .. Action_Vampire_Gaze_Blocked;
	subtype Doctor_Message_Kind is Message_Kind range Doctor_Cure .. Doctor_Found_Gremlin_Preview;
	subtype Detective_Message_Kind is Message_Kind range Detective_Survey .. Detective_Survey_Victim;
	subtype Hunter_Message_Kind is Message_Kind range Hunter_Guard .. Hunter_Failed_With_Silver;
	subtype Vampire_Message_Kind is Message_Kind range Vampire_Murder .. Vampire_Failed_And_Killed;
	subtype Servant_Message_Kind is Message_Kind range Servant_Knew_Vampire_K .. Servant_Knew_Vampires;
	
	type Message is record
		Day : Integer;
		Time : Ada.Calendar.Time;
		Kind : Message_Kind;
		Subject : Integer;
		Target : Integer;
		Text : Ada.Strings.Unbounded.Unbounded_String;
	end record;
	
	Default_Message : constant Message := (
		Day => -1,
		Time => Calendar.Null_Time,
		Kind => Narration,
		Subject => -1,
		Target => -1,
		Text => Ada.Strings.Unbounded.Null_Unbounded_String);
	
	package Messages is new Ada.Containers.Vectors (Natural, Message);
	
	type Message_Count is record
		Speech, Monologue, Ghost, Wake, Encourage, Encouraged, Vampire_Gaze : Natural;
		Last_Action_Time : Ada.Calendar.Time ;
	end record;
	type Message_Counts is array(Natural range <>) of Message_Count;
	
	type Village_Type is new Tabula.Villages.Village with record
		Name : Ada.Strings.Unbounded.Unbounded_String;
		By : Ada.Strings.Unbounded.Unbounded_String;
		State : Tabula.Villages.Village_State;
		Today : Integer;
		Time : Tabula.Villages.Village_Time := Tabula.Villages.Daytime;
		Dawn : Ada.Calendar.Time;
		Day_Duration : Duration := Default_Long_Day_Duration;
		Night_Duration : Duration := Default_Night_Duration;
		Execution : Execution_Mode := From_Second;
		Teaming : Villages.Teaming := Shuffling_Headless;
		Monster_Side : Villages.Monster_Side := Fixed;
		Attack : Attack_Mode := Two;
		Servant_Knowing : Servant_Knowing_Mode := None;
		Daytime_Preview : Daytime_Preview_Mode := Role_And_Message;
		Doctor_Infected : Doctor_Infected_Mode := Cure;
		Hunter_Silver_Bullet : Hunter_Silver_Bullet_Mode := Target_And_Self;
		Unfortunate : Unfortunate_Mode := None;
		Appearance : Role_Appearances := (others => Random);
		Dummy_Role : aliased Person_Role := Inhabitant;
		People : aliased Villages.People.Vector;
		Escaped_People : aliased Villages.People.Vector;
		Messages : aliased Villages.Messages.Vector;
	end record;
	
	function Count_Messages(Village : Village_Type; Day : Natural) return Message_Counts;
	function Count_Speech(Village : Village_Type; Day : Natural) return Natural;
	function Last_Joined_Time(Village : Village_Type) return Ada.Calendar.Time;
	
	function Joined(Village : Village_Type; User_Id : String) return Integer;
	function Rejoined(Village : Village_Type; Escaped_Subject : Natural) return Integer;
	
	function Be_Voting (Village : Village_Type) return Boolean;
	function Provisional_Voted(Village : Village_Type) return Boolean;
	function Vote_Finished(Village : Village_Type) return Boolean;
	function No_Commit (Village : Village_Type) return Boolean;
	function Commit_Finished(Village : Village_Type) return Boolean;
	function Find_Superman(Village : Village_Type; Role : Person_Role) return Integer;
	function Unfortunate(Village : Village_Type) return Boolean;
	
	procedure Escape(Village : in out Village_Type; Subject : Natural; Time : Ada.Calendar.Time);
	procedure Vote(Village : in out Village_Type; Player : Natural; Target : Integer; Apply: Boolean; Time : Ada.Calendar.Time);
	
	function Already_Joined_Another_Sex(Village : Village_Type; User_Id : String; Sex : Casts.Sex_Kind) return Boolean;
	
	function Escape_Duration(Village : Village_Type) return Duration;
	
	procedure Exclude_Taken (Cast : in out Casts.Cast_Collection; Village : in Village_Type);
	
	overriding procedure Iterate (
		Village : not null access constant Village_Type;
		Process : not null access procedure (Item : in Tabula.Villages.Root_Option_Item'Class));
	
	package Options is
		
		package Day_Duration is
			type Option_Item (Village : not null access constant Village_Type) is
				new Tabula.Villages.Root_Option_Item with null record;
			function Available (Item : Option_Item) return Boolean;
			function Name (Item : Option_Item) return String;
			function Changed (Item : Option_Item) return Boolean;
			procedure Iterate (
				Item : in Option_Item;
				Process : not null access procedure (
					Value : in String;
					Selected : in Boolean;
					Message : in String;
					Unrecommended : in Boolean));
			procedure Change (
				Village : not null access Tabula.Villages.Village'Class;
				Item : in Option_Item;
				Value : in String);
		end Day_Duration;
		
		package Night_Duration is
			type Option_Item (Village : not null access constant Village_Type) is
				new Tabula.Villages.Root_Option_Item with null record;
			function Available (Item : Option_Item) return Boolean;
			function Name (Item : Option_Item) return String;
			function Changed (Item : Option_Item) return Boolean;
			procedure Iterate (
				Item : in Option_Item;
				Process : not null access procedure (
					Value : in String;
					Selected : in Boolean;
					Message : in String;
					Unrecommended : in Boolean));
			procedure Change (
				Village : not null access Tabula.Villages.Village'Class;
				Item : in Option_Item;
				Value : in String);
		end Night_Duration;
		
		package Execution is
			type Option_Item (Village : not null access constant Village_Type) is
				new Tabula.Villages.Root_Option_Item with null record;
			function Available (Item : Option_Item) return Boolean;
			function Name (Item : Option_Item) return String;
			function Changed (Item : Option_Item) return Boolean;
			procedure Iterate (
				Item : in Option_Item;
				Process : not null access procedure (
					Value : in String;
					Selected : in Boolean;
					Message : in String;
					Unrecommended : in Boolean));
			procedure Change (
				Village : not null access Tabula.Villages.Village'Class;
				Item : in Option_Item;
				Value : in String);
		end Execution;
		
		package Teaming is
			type Option_Item (Village : not null access constant Village_Type) is
				new Tabula.Villages.Root_Option_Item with null record;
			function Available (Item : Option_Item) return Boolean;
			function Name (Item : Option_Item) return String;
			function Changed (Item : Option_Item) return Boolean;
			procedure Iterate (
				Item : in Option_Item;
				Process : not null access procedure (
					Value : in String;
					Selected : in Boolean;
					Message : in String;
					Unrecommended : in Boolean));
			procedure Change (
				Village : not null access Tabula.Villages.Village'Class;
				Item : in Option_Item;
				Value : in String);
		end Teaming;
		
		package Monster_Side is
			type Option_Item (Village : not null access constant Village_Type) is
				new Tabula.Villages.Root_Option_Item with null record;
			function Available (Item : Option_Item) return Boolean;
			function Name (Item : Option_Item) return String;
			function Changed (Item : Option_Item) return Boolean;
			procedure Iterate (
				Item : in Option_Item;
				Process : not null access procedure (
					Value : in String;
					Selected : in Boolean;
					Message : in String;
					Unrecommended : in Boolean));
			procedure Change (
				Village : not null access Tabula.Villages.Village'Class;
				Item : in Option_Item;
				Value : in String);
		end Monster_Side;
		
		package Attack is
			type Option_Item (Village : not null access constant Village_Type) is
				new Tabula.Villages.Root_Option_Item with null record;
			function Available (Item : Option_Item) return Boolean;
			function Name (Item : Option_Item) return String;
			function Changed (Item : Option_Item) return Boolean;
			procedure Iterate (
				Item : in Option_Item;
				Process : not null access procedure (
					Value : in String;
					Selected : in Boolean;
					Message : in String;
					Unrecommended : in Boolean));
			procedure Change (
				Village : not null access Tabula.Villages.Village'Class;
				Item : in Option_Item;
				Value : in String);
		end Attack;
		
		package Servant_Knowing is
			type Option_Item (Village : not null access constant Village_Type) is
				new Tabula.Villages.Root_Option_Item with null record;
			function Available (Item : Option_Item) return Boolean;
			function Name (Item : Option_Item) return String;
			function Changed (Item : Option_Item) return Boolean;
			procedure Iterate (
				Item : in Option_Item;
				Process : not null access procedure (
					Value : in String;
					Selected : in Boolean;
					Message : in String;
					Unrecommended : in Boolean));
			procedure Change (
				Village : not null access Tabula.Villages.Village'Class;
				Item : in Option_Item;
				Value : in String);
		end Servant_Knowing;
		
		package Daytime_Preview is
			type Option_Item (Village : not null access constant Village_Type) is
				new Tabula.Villages.Root_Option_Item with null record;
			function Available (Item : Option_Item) return Boolean;
			function Name (Item : Option_Item) return String;
			function Changed (Item : Option_Item) return Boolean;
			procedure Iterate (
				Item : in Option_Item;
				Process : not null access procedure (
					Value : in String;
					Selected : in Boolean;
					Message : in String;
					Unrecommended : in Boolean));
			procedure Change (
				Village : not null access Tabula.Villages.Village'Class;
				Item : in Option_Item;
				Value : in String);
		end Daytime_Preview;
		
		package Doctor_Infected is
			type Option_Item (Village : not null access constant Village_Type) is
				new Tabula.Villages.Root_Option_Item with null record;
			function Available (Item : Option_Item) return Boolean;
			function Name (Item : Option_Item) return String;
			function Changed (Item : Option_Item) return Boolean;
			procedure Iterate (
				Item : in Option_Item;
				Process : not null access procedure (
					Value : in String;
					Selected : in Boolean;
					Message : in String;
					Unrecommended : in Boolean));
			procedure Change (
				Village : not null access Tabula.Villages.Village'Class;
				Item : in Option_Item;
				Value : in String);
		end Doctor_Infected;
		
		package Hunter_Silver_Bullet is
			type Option_Item (Village : not null access constant Village_Type) is
				new Tabula.Villages.Root_Option_Item with null record;
			function Available (Item : Option_Item) return Boolean;
			function Name (Item : Option_Item) return String;
			function Changed (Item : Option_Item) return Boolean;
			procedure Iterate (
				Item : in Option_Item;
				Process : not null access procedure (
					Value : in String;
					Selected : in Boolean;
					Message : in String;
					Unrecommended : in Boolean));
			procedure Change (
				Village : not null access Tabula.Villages.Village'Class;
				Item : in Option_Item;
				Value : in String);
		end Hunter_Silver_Bullet;
		
		package Unfortunate is
			type Option_Item (Village : not null access constant Village_Type) is
				new Tabula.Villages.Root_Option_Item with null record;
			function Available (Item : Option_Item) return Boolean;
			function Name (Item : Option_Item) return String;
			function Changed (Item : Option_Item) return Boolean;
			procedure Iterate (
				Item : in Option_Item;
				Process : not null access procedure (
					Value : in String;
					Selected : in Boolean;
					Message : in String;
					Unrecommended : in Boolean));
			procedure Change (
				Village : not null access Tabula.Villages.Village'Class;
				Item : in Option_Item;
				Value : in String);
		end Unfortunate;
		
	end Options;
	
end Tabula.Vampires.Villages;

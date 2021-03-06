-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Calendar;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Tabula.Calendar;
with Tabula.Casts;
with Tabula.Villages;
package Vampire.Villages is
	use Tabula.Villages;
	
	Default_Long_Day_Duration : constant Duration := 2 * 24 * 60 * 60 * 1.0;
	Default_Short_Day_Duration : constant Duration := 15 * 60.0;
	Default_Night_Duration : constant Duration := 0.0;
	Epilogue_Min_Duration : constant Duration := 60 * 60.0;
	Vote_Duration : constant Duration := 5 * 60.0;
	Speech_Simultaneous : constant Duration := 30.0;
	
	Minimum_Number_Of_Persons : constant := 7;
	Maximum_Number_Of_Persons : constant := 16;
	
	Speech_Limit : constant := 20;
	Encouraged_Speech_Limit : constant := 2;
	Ghost_Limit : constant := 10;
	Monologue_Limit : constant := 10;
	
	Max_Length_Of_Message : constant := 1024;
	
	-- オプションルール
	
	type Vote_Mode is (Unsigned, Preliminary_And_Final);
	type Execution_Mode is (
		Dummy_Killed_And_From_First, Infection_And_From_First, From_First,
		From_Second);
	type Formation_Mode is (Public, Hidden);
	type Attack_Mode is (Two, Nocturnal_Chain_Infecting, Unanimity);
	type Vampire_Action_Set_Mode is (None, Gaze, Gaze_And_Cancel);
	type Servant_Knowing_Mode is (None, Vampire_K, All_Vampires);
	type Monster_Side_Mode is (Fixed, Shuffling, Gremlin);
	type Daytime_Preview_Mode is (None, Role_Only, Message_Only, Role_And_Message);
	type Doctor_Infected_Mode is (Cure, Find_Infection);
	type Hunter_Silver_Bullet_Mode is (Target, Target_And_Self);
	type Unfortunate_Mode is (None, Appear, Infected_Only);
	
	type Obsolete_Teaming_Mode is (
		Low_Density, Liner_2, Shuffling_Headless, Shuffling_Euro, Shuffling,
		Shuffling_Gremlin, Hiding, Hiding_Gremlin);
	
	Initial_Vote                 : constant Vote_Mode                 := Unsigned;
	Initial_Execution            : constant Execution_Mode            := From_First;
	Initial_Formation            : constant Formation_Mode            := Public;
	Initial_Monster_Side         : constant Monster_Side_Mode         := Fixed;
	Initial_Attack               : constant Attack_Mode               := Nocturnal_Chain_Infecting;
	Initial_Vampire_Action_Set   : constant Vampire_Action_Set_Mode   := Gaze;
	Initial_Servant_Knowing      : constant Servant_Knowing_Mode      := Vampire_K;
	Initial_Daytime_Preview      : constant Daytime_Preview_Mode      := Message_Only;
	Initial_Doctor_Infected      : constant Doctor_Infected_Mode      := Find_Infection;
	Initial_Hunter_Silver_Bullet : constant Hunter_Silver_Bullet_Mode := Target_And_Self;
	Initial_Unfortunate          : constant Unfortunate_Mode          := Infected_Only;
	
	Initial_Obsolete_Teaming     : constant Obsolete_Teaming_Mode     := Shuffling;
	
	Is_Obsolete_Teaming : constant array (Obsolete_Teaming_Mode) of Boolean := (
		Liner_2 | Shuffling | Hiding => False,
		Low_Density | Shuffling_Headless | Shuffling_Euro | Shuffling_Gremlin
		| Hiding_Gremlin => True);
	
	-- 配役
	
	type Person_Role is (
		Vampire_K, Vampire_Q, Vampire_J, Servant,
		Inhabitant,
		Loved_Inhabitant,
		Unfortunate_Inhabitant,
		Detective, Doctor, Astronomer, Hunter,
		Lover, Sweetheart_M, Sweetheart_F,
		Gremlin);
	
	subtype Matrix_Role is Person_Role range Detective .. Hunter;
	subtype Night_Role is Person_Role range Astronomer .. Hunter;
	subtype Daytime_Role is Person_Role range Detective .. Doctor;
	subtype Vampire_Role is Person_Role range Vampire_K .. Vampire_J;
	
	type Role_Appearance is (None, Random, Force);
	type Role_Appearances is
		array (Unfortunate_Inhabitant .. Lover) of Role_Appearance;
	
	type Role_Images is
		array (Villages.Person_Role) of not null access constant String;
	
	type Ability_State is (Disallowed, Allowed, Already_Used);
	
	-- 参加者
	
	type Requested_Role is (Random, Rest,
		Inhabitant, Detective, Astronomer, Doctor, Hunter, Sweetheart,
		Servant, Vampire,
		Village_Side, Vampire_Side, Gremlin);
	
	type Person_State is (Normal, Infected, Died);
	
	type Person_Record is record
		State : Person_State;
		Vote : Person_Index'Base;
		Provisional_Vote : Person_Index'Base; -- 仮投票
		Candidate : Boolean; -- 投票の候補
		Target : Person_Index'Base;
		Special : Boolean;
		Note : aliased Ada.Strings.Unbounded.Unbounded_String;
	end record;
	
	Default_Person_Record : constant Person_Record := (
		State => Normal,
		Vote => No_Person,
		Provisional_Vote => No_Person,
		Candidate => True,
		Target => No_Person,
		Special => False,
		Note => Ada.Strings.Unbounded.Null_Unbounded_String);
	
	package Person_Records is new Ada.Containers.Vectors (Natural, Person_Record);
	
	type Person_Type is new Tabula.Villages.Person_Type with record
		Request : Requested_Role;
		Ignore_Request : Boolean;
		Role : Person_Role;
		Records : aliased Person_Records.Vector;
		Commited : Boolean;
	end record;
	
	Empty_Person : constant Person_Type := (Casts.Empty_Person with
		Id => Ada.Strings.Unbounded.Null_Unbounded_String,
		Request => Random,
		Ignore_Request => False,
		Role => Inhabitant,
		Records => Person_Records.Empty_Vector,
		Commited => False);
	
	package People is new Ada.Containers.Vectors (Person_Index, Person_Type);
	
	-- ログ
	
	type Message_Kind is (
		Narration,                      -- ト書き
		Escape,                         -- 村を出る
		Escaped_Join,                   -- 村を出た者の参加
		Escaped_Speech,                 -- 村を出た者の会話
		Join,                           -- 参加
		Speech,                         -- 通常会話
		Monologue,                      -- 独り言
		Ghost,                          -- 墓場
		Howling,                        -- 夜間の会話
		Howling_Blocked,                -- 夜間の会話が妨害された
		Action_Wake,                    -- 起こす
		Action_Encourage,               -- 促し
		Action_Vampire_Gaze,            -- 視線
		Action_Vampire_Gaze_Blocked,    -- 視線が妨害された
		Action_Vampire_Cancel,          -- 襲撃取り消し
		Action_Vampire_Canceled,        -- 自分の襲撃設定が取り消された
		Doctor_Cure,                    -- 治療
		Doctor_Cure_Preview,            -- 治療
		Doctor_Found_Infection,         -- 感染を発見しただけ
		Doctor_Found_Infection_Preview, -- 感染を発見しただけ
		Doctor_Failed,                  -- 診察はしたが感染させられた患者では無かった
		Doctor_Failed_Preview,          -- 診察はしたが感染させられた患者では無かった
		Doctor_Found_Gremlin,           -- 妖魔を見つけた
		Doctor_Found_Gremlin_Preview,   -- 妖魔を見つけた
		Detective_Survey,               -- 調査
		Detective_Survey_Preview,       -- 調査
		Detective_Survey_Victim,        -- 初日犠牲者の調査
		Preliminary_Vote,               -- 一次開票
		Execution,                      -- 処刑
		Awareness,                      -- 自覚
		Astronomer_Observation,         -- 観測
		Meeting,                        -- 吸血鬼の会話
		Vampire_Infection_In_First,     -- 初日の感染
		Vampire_Failed_In_First,        -- 初日の感染に失敗
		Vampire_Murder,                 -- 襲撃
		Vampire_Murder_And_Killed,      -- 襲撃に成功し相打ちで銀の弾丸を撃ちこまれた
		Vampire_Infection,              -- 感染
		Vampire_Infection_And_Killed,   -- 感染に成功し相打ちで銀の弾丸を撃ちこまれた
		Vampire_Failed,                 -- 襲撃に失敗
		Vampire_Failed_And_Killed,      -- 襲撃に失敗し銀の弾丸を撃ちこまれた
		Hunter_Guard_No_Response,       -- 護衛していた(手応えなし)
		Hunter_Guard,                   -- 護衛に成功した
		Hunter_Guard_With_Silver,       -- 護衛に成功し銀の弾丸を撃ちこんだ
		Hunter_Nothing_With_Silver,     -- 銀の弾丸を込めていたが何も無かった
		Hunter_Infected_With_Silver,    -- 誰かを護衛していたわけではないが自分が襲われたので銀の弾丸で反撃した
		Hunter_Killed_With_Silver,      -- 銀の弾丸で相打ち
		Hunter_Failed,                  -- ガードしたが吸血鬼は来なかった
		Hunter_Failed_With_Silver,      -- ガードしたが吸血鬼は来ず銀の弾丸を無駄遣いした
		Gremlin_Sense,                  -- 妖魔が吸血鬼の残数を知る
		Sweetheart_Incongruity,         -- 違和感
		Sweetheart_Suicide,             -- 後追い
		Servant_Knew_Vampire_K,         -- Kを知る
		Servant_Knew_Vampires,          -- 吸血鬼全員を知る
		List,                           -- 一覧
		Foreboding,                     -- 初日感染の公開メッセージ
		Introduction,                   -- 序文
		Breakdown);                     -- 開始
	
	subtype Action_Message_Kind is
		Message_Kind range Action_Wake .. Action_Vampire_Gaze_Blocked;
	
	subtype Doctor_Message_Kind is
		Message_Kind range Doctor_Cure .. Doctor_Found_Gremlin_Preview;
	subtype Detective_Message_Kind is
		Message_Kind range Detective_Survey .. Detective_Survey_Victim;
	subtype Hunter_Message_Kind is
		Message_Kind range Hunter_Guard_No_Response .. Hunter_Failed_With_Silver;
	subtype Vampire_Message_Kind is
		Message_Kind range Vampire_Infection_In_First .. Vampire_Failed_And_Killed;
	subtype Servant_Message_Kind is
		Message_Kind range Servant_Knew_Vampire_K .. Servant_Knew_Vampires;
	
	type Message is record
		Day : Integer;
		Time : Ada.Calendar.Time;
		Kind : Message_Kind;
		Subject : Person_Index'Base;
		Target : Person_Index'Base;
		Text : aliased Ada.Strings.Unbounded.Unbounded_String;
	end record;
	
	Default_Message : constant Message := (
		Day => -1,
		Time => Calendar.Null_Time,
		Kind => Narration,
		Subject => No_Person,
		Target => No_Person,
		Text => Ada.Strings.Unbounded.Null_Unbounded_String);
	
	package Messages is new Ada.Containers.Vectors (Message_Index, Message);
	
	type Message_Count is record
		Speech, Monologue, Ghost : Natural;
		Wake, Encourage, Encouraged, Vampire_Gaze, Vampire_Cancel : Natural;
		Last_Action_Time : Ada.Calendar.Time ;
	end record;
	type Message_Counts is array (Person_Index range <>) of Message_Count;
	
	type Voted_Counts is array (Person_Index range <>) of Natural;
	type Voted_Count_Info (Last : Person_Index'Base) is record
		Max : Natural;
		Counts : Voted_Counts (Person_Index'First .. Last);
	end record;
	
	type Vote_State_Type is (Disallowed, Allowed_For_Preliminary, Allowed);
	
	-- 村
	
	type Village_Time is (Daytime, Vote, Night);
	
	type Village_Type is limited new Tabula.Villages.Village_Type with record
		Day_Duration : Duration := Default_Long_Day_Duration;
		Night_Duration : Duration := Default_Night_Duration;
		State : Village_State := Prologue;
		Today : Integer := 0;
		Time : Village_Time := Daytime;
		Dawn : Ada.Calendar.Time := Calendar.Null_Time;
			-- 更新時刻(1日目は夜を飛ばすため調整)
		Vote : Vote_Mode := Unsigned;
		Execution : Execution_Mode := From_First;
		Formation : Formation_Mode := Public;
		Monster_Side : Monster_Side_Mode := Fixed;
		Attack : Attack_Mode := Two;
		Vampire_Action_Set : Vampire_Action_Set_Mode := Gaze;
		Servant_Knowing : Servant_Knowing_Mode := None;
		Daytime_Preview : Daytime_Preview_Mode := Role_And_Message;
		Doctor_Infected : Doctor_Infected_Mode := Cure;
		Hunter_Silver_Bullet : Hunter_Silver_Bullet_Mode := Target_And_Self;
		Unfortunate : Unfortunate_Mode := None;
		Obsolete_Teaming : Obsolete_Teaming_Mode := Shuffling_Headless;
		Appearance : Role_Appearances := (others => Random);
		Dummy_Role : aliased Person_Role := Inhabitant;
		People : aliased Villages.People.Vector;
		Escaped_People : aliased Villages.People.Vector;
		Messages : aliased Villages.Messages.Vector;
	end record;
	
	-- 作成
	function Create (
		Name : String;
		By : String;
		Term : Village_Term;
		Time : Ada.Calendar.Time)
		return Village_Type;
	
	-- 発言数
	function Count_Messages (Village : Village_Type; Day : Natural)
		return Message_Counts;
	
	-- 更新予定時刻
	function Night_To_Daytime (Village : Village_Type) return Ada.Calendar.Time;
	function Infection_In_First_Time (Village : Village_Type)
		return Ada.Calendar.Time;
	function Preliminary_Vote_Time (Village : Village_Type)
		return Ada.Calendar.Time;
	function Daytime_To_Vote (Village : Village_Type) return Ada.Calendar.Time;
	function Vote_To_Night (Village : Village_Type) return Ada.Calendar.Time;
	
	function No_Commit (Village : Village_Type) return Boolean;
	function Commit_Finished (Village : Village_Type) return Boolean;
	
	-- 参加
	procedure Join (
		Village : in out Village_Type;
		Id : in String;
		Group : in Casts.Group;
		Figure : in Casts.Person;
		Work : in Casts.Work;
		Request : in Requested_Role;
		Ignore_Request : in Boolean;
		Time : in Ada.Calendar.Time);
	
	-- 村抜け
	function Escape_Duration (Village : Village_Type) return Duration;
	procedure Escape (
		Village : in out Village_Type;
		Subject : in Person_Index;
		Time : in Ada.Calendar.Time);
	
	-- 投票
	function Vote_State (Village : Village_Type) return Vote_State_Type;
	function Vote_Finished (Village : Village_Type) return Boolean;
	function Voted_Count (
		Village : Village_Type;
		Day : Natural;
		Preliminary : Boolean)
		return Voted_Count_Info;
	procedure Vote (
		Village : in out Village_Type;
		Subject : in Person_Index;
		Target : in Person_Index'Base);
	
	-- アクション
	function Can_Gaze (Village : Village_Type) return Boolean;
	
	procedure Wake (
		Village : in out Village_Type;
		Subject : in Person_Index;
		Target : in Person_Index;
		Time : in Ada.Calendar.Time);
	procedure Encourage (
		Village : in out Village_Type;
		Subject : in Person_Index;
		Target : in Person_Index;
		Time : in Ada.Calendar.Time);
	procedure Vampire_Gaze (
		Village : in out Village_Type;
		Subject : in Person_Index;
		Target : in Person_Index;
		Time : in Ada.Calendar.Time);
	procedure Vampire_Cancel (
		Village : in out Village_Type;
		Subject : in Person_Index;
		Target : in Person_Index;
		Time : in Ada.Calendar.Time);
	
	-- 能力者
	function Infected_In_First (Village : Village_Type) return Boolean;
	function Is_Anyone_Died (Village : Village_Type; Day : Natural)
		return Boolean;
	function Find_Superman (Village : Village_Type; Role : Person_Role)
		return Person_Index'Base;
	function Target_Day (Village : Village_Type) return Integer;
	function Astronomer_Target_Day (Village : Village_Type) return Integer;
	function Already_Used_Special (Village : Village_Type; Subject : Person_Index)
		return Boolean;
	
	function Detective_State (Village : Village_Type; Subject : Person_Index)
		return Ability_State;
	function Doctor_State (Village : Village_Type; Subject : Person_Index)
		return Ability_State;
	function Superman_State (Village : Village_Type; Subject : Person_Index)
		return Ability_State;
	function Silver_Bullet_State (Village : Village_Type; Subject : Person_Index)
		return Ability_State;
	
	procedure Select_Target (
		Village : in out Village_Type;
		Subject : in Person_Index;
		Target : in Person_Index'Base;
		Special : in Boolean := False;
		Time : in Ada.Calendar.Time);
	
	procedure Night_Talk (
		Village : in out Village_Type;
		Subject : in Person_Index;
		Text : in String;
		Time : in Ada.Calendar.Time);
	
	-- 会話
	procedure Speech (
		Village : in out Village_Type;
		Subject : in Person_Index;
		Text : in String;
		Time : in Ada.Calendar.Time);
	procedure Monologue (
		Village : in out Village_Type;
		Subject : in Person_Index;
		Text : in String;
		Time : in Ada.Calendar.Time);
	procedure Ghost (
		Village : in out Village_Type;
		Subject : in Person_Index;
		Text : in String;
		Time : in Ada.Calendar.Time);
	
	-- Administrator操作
	procedure Narration (
		Village : in out Village_Type;
		Text : in String;
		Time : in Ada.Calendar.Time);
	
	overriding function Term (Village : Village_Type) return Village_Term;
	
	overriding procedure Get_State (
		Village : in Village_Type;
		State : out Village_State;
		Today : out Natural);
	
	overriding procedure Iterate_People (
		Village : in Village_Type;
		Process : not null access procedure (
			Index : Person_Index;
			Item : in Tabula.Villages.Person_Type'Class));
	
	overriding procedure Iterate_Escaped_People (
		Village : in Village_Type;
		Process : not null access procedure (
			Index : Person_Index;
			Item : in Tabula.Villages.Person_Type'Class));

	overriding function Speech_Range (
		Village : Village_Type;
		Day : Natural)
		return Speech_Range_Type;
	
	overriding function Recent_Only_Speech_Range (
		Village : Village_Type;
		Day : Natural;
		Now : Ada.Calendar.Time)
		return Speech_Range_Type;
	
	overriding procedure Iterate_Options (
		Village : in Village_Type;
		Process : not null access procedure (
			Item : in Root_Option_Item'Class));
	
	package Options is
		
		package Day_Duration is
			type Option_Item (Village : not null access constant Village_Type) is
				new Root_Option_Item with null record;
			overriding function Available (Item : Option_Item) return Boolean;
			overriding function Name (Item : Option_Item) return String;
			overriding function Changed (Item : Option_Item) return Boolean;
			overriding procedure Iterate (
				Item : in Option_Item;
				Process : not null access procedure (
					Value : in String;
					Selected : in Boolean;
					Message : in String;
					Unrecommended : in Boolean));
			overriding procedure Change (
				Village : in out Tabula.Villages.Village_Type'Class;
				Item : in Option_Item;
				Value : in String);
		end Day_Duration;
		
		package Night_Duration is
			type Option_Item (Village : not null access constant Village_Type) is
				new Root_Option_Item with null record;
			overriding function Available (Item : Option_Item) return Boolean;
			overriding function Name (Item : Option_Item) return String;
			overriding function Changed (Item : Option_Item) return Boolean;
			overriding procedure Iterate (
				Item : in Option_Item;
				Process : not null access procedure (
					Value : in String;
					Selected : in Boolean;
					Message : in String;
					Unrecommended : in Boolean));
			overriding procedure Change (
				Village : in out Tabula.Villages.Village_Type'Class;
				Item : in Option_Item;
				Value : in String);
		end Night_Duration;
		
		package Vote is
			type Option_Item (Village : not null access constant Village_Type) is
				new Root_Option_Item with null record;
			overriding function Available (Item : Option_Item) return Boolean;
			overriding function Name (Item : Option_Item) return String;
			overriding function Changed (Item : Option_Item) return Boolean;
			overriding procedure Iterate (
				Item : in Option_Item;
				Process : not null access procedure (
					Value : in String;
					Selected : in Boolean;
					Message : in String;
					Unrecommended : in Boolean));
			overriding procedure Change (
				Village : in out Tabula.Villages.Village_Type'Class;
				Item : in Option_Item;
				Value : in String);
		end Vote;
		
		package Execution is
			type Option_Item (Village : not null access constant Village_Type) is
				new Root_Option_Item with null record;
			overriding function Available (Item : Option_Item) return Boolean;
			overriding function Name (Item : Option_Item) return String;
			overriding function Changed (Item : Option_Item) return Boolean;
			overriding procedure Iterate (
				Item : in Option_Item;
				Process : not null access procedure (
					Value : in String;
					Selected : in Boolean;
					Message : in String;
					Unrecommended : in Boolean));
			overriding procedure Change (
				Village : in out Tabula.Villages.Village_Type'Class;
				Item : in Option_Item;
				Value : in String);
		end Execution;
		
		package Formation is
			type Option_Item (Village : not null access constant Village_Type) is
				new Root_Option_Item with null record;
			overriding function Available (Item : Option_Item) return Boolean;
			overriding function Name (Item : Option_Item) return String;
			overriding function Changed (Item : Option_Item) return Boolean;
			overriding procedure Iterate (
				Item : in Option_Item;
				Process : not null access procedure (
					Value : in String;
					Selected : in Boolean;
					Message : in String;
					Unrecommended : in Boolean));
			overriding procedure Change (
				Village : in out Tabula.Villages.Village_Type'Class;
				Item : in Option_Item;
				Value : in String);
		end Formation;
		
		package Monster_Side is
			type Option_Item (Village : not null access constant Village_Type) is
				new Root_Option_Item with null record;
			overriding function Available (Item : Option_Item) return Boolean;
			overriding function Name (Item : Option_Item) return String;
			overriding function Changed (Item : Option_Item) return Boolean;
			overriding procedure Iterate (
				Item : in Option_Item;
				Process : not null access procedure (
					Value : in String;
					Selected : in Boolean;
					Message : in String;
					Unrecommended : in Boolean));
			overriding procedure Change (
				Village : in out Tabula.Villages.Village_Type'Class;
				Item : in Option_Item;
				Value : in String);
		end Monster_Side;
		
		package Attack is
			type Option_Item (Village : not null access constant Village_Type) is
				new Root_Option_Item with null record;
			overriding function Available (Item : Option_Item) return Boolean;
			overriding function Name (Item : Option_Item) return String;
			overriding function Changed (Item : Option_Item) return Boolean;
			overriding procedure Iterate (
				Item : in Option_Item;
				Process : not null access procedure (
					Value : in String;
					Selected : in Boolean;
					Message : in String;
					Unrecommended : in Boolean));
			overriding procedure Change (
				Village : in out Tabula.Villages.Village_Type'Class;
				Item : in Option_Item;
				Value : in String);
		end Attack;
		
		package Vampire_Action_Set is
			type Option_Item (Village : not null access constant Village_Type) is
				new Root_Option_Item with null record;
			overriding function Available (Item : Option_Item) return Boolean;
			overriding function Name (Item : Option_Item) return String;
			overriding function Changed (Item : Option_Item) return Boolean;
			overriding procedure Iterate (
				Item : in Option_Item;
				Process : not null access procedure (
					Value : in String;
					Selected : in Boolean;
					Message : in String;
					Unrecommended : in Boolean));
			overriding procedure Change (
				Village : in out Tabula.Villages.Village_Type'Class;
				Item : in Option_Item;
				Value : in String);
		end Vampire_Action_Set;
		
		package Servant_Knowing is
			type Option_Item (Village : not null access constant Village_Type) is
				new Root_Option_Item with null record;
			overriding function Available (Item : Option_Item) return Boolean;
			overriding function Name (Item : Option_Item) return String;
			overriding function Changed (Item : Option_Item) return Boolean;
			overriding procedure Iterate (
				Item : in Option_Item;
				Process : not null access procedure (
					Value : in String;
					Selected : in Boolean;
					Message : in String;
					Unrecommended : in Boolean));
			overriding procedure Change (
				Village : in out Tabula.Villages.Village_Type'Class;
				Item : in Option_Item;
				Value : in String);
		end Servant_Knowing;
		
		package Daytime_Preview is
			type Option_Item (Village : not null access constant Village_Type) is
				new Root_Option_Item with null record;
			overriding function Available (Item : Option_Item) return Boolean;
			overriding function Name (Item : Option_Item) return String;
			overriding function Changed (Item : Option_Item) return Boolean;
			overriding procedure Iterate (
				Item : in Option_Item;
				Process : not null access procedure (
					Value : in String;
					Selected : in Boolean;
					Message : in String;
					Unrecommended : in Boolean));
			overriding procedure Change (
				Village : in out Tabula.Villages.Village_Type'Class;
				Item : in Option_Item;
				Value : in String);
		end Daytime_Preview;
		
		package Doctor_Infected is
			type Option_Item (Village : not null access constant Village_Type) is
				new Root_Option_Item with null record;
			overriding function Available (Item : Option_Item) return Boolean;
			overriding function Name (Item : Option_Item) return String;
			overriding function Changed (Item : Option_Item) return Boolean;
			overriding procedure Iterate (
				Item : in Option_Item;
				Process : not null access procedure (
					Value : in String;
					Selected : in Boolean;
					Message : in String;
					Unrecommended : in Boolean));
			overriding procedure Change (
				Village : in out Tabula.Villages.Village_Type'Class;
				Item : in Option_Item;
				Value : in String);
		end Doctor_Infected;
		
		package Hunter_Silver_Bullet is
			type Option_Item (Village : not null access constant Village_Type) is
				new Root_Option_Item with null record;
			overriding function Available (Item : Option_Item) return Boolean;
			overriding function Name (Item : Option_Item) return String;
			overriding function Changed (Item : Option_Item) return Boolean;
			overriding procedure Iterate (
				Item : in Option_Item;
				Process : not null access procedure (
					Value : in String;
					Selected : in Boolean;
					Message : in String;
					Unrecommended : in Boolean));
			overriding procedure Change (
				Village : in out Tabula.Villages.Village_Type'Class;
				Item : in Option_Item;
				Value : in String);
		end Hunter_Silver_Bullet;
		
		package Unfortunate is
			type Option_Item (Village : not null access constant Village_Type) is
				new Root_Option_Item with null record;
			overriding function Available (Item : Option_Item) return Boolean;
			overriding function Name (Item : Option_Item) return String;
			overriding function Changed (Item : Option_Item) return Boolean;
			overriding procedure Iterate (
				Item : in Option_Item;
				Process : not null access procedure (
					Value : in String;
					Selected : in Boolean;
					Message : in String;
					Unrecommended : in Boolean));
			overriding procedure Change (
				Village : in out Tabula.Villages.Village_Type'Class;
				Item : in Option_Item;
				Value : in String);
		end Unfortunate;
		
	end Options;
	
end Vampire.Villages;

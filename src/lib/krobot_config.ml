(*
 * krobot_config.ml
 * ----------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

let sqr x = x *. x

let world_height = 2.
let world_width = 3.
let robot_length = 0.245
let robot_width = 0.30
let wheels_diameter = (0.02475843 *. 2.)
let wheels_distance = 0.15474249
let wheels_position = 0.165
let robot_radius = wheels_position +. 0.02
let rotary_beacon_index_pos = 0.

let safety_margin = 0.02

let beacon_radius = 0.2

let fire_radius = 0.07

open Krobot_geom

let pi = 4. *. atan 1.

let green_initial_position =
  { x = (world_width -. robot_radius -. 0.07);
    y = world_height /. 2.; },
  (pi)

let yellow_initial_position =
  { x = 0.07 +. robot_radius;
    y = world_height /. 2.; },
  (pi)

let green_fixed_beacons = [
  { x = -. 0.062; y = world_height +. 0.062 };
  { x = -. 0.062; y = -. 0.062 };
  { x = world_width +. 0.062; y = world_height /. 2.}]

let yellow_fixed_beacons = [
  { x = world_width +. 0.062; y = world_height +. 0.062 };
  { x = world_width +. 0.062; y = -. 0.062 };
  { x = -. 0.062; y = world_height /. 2.}]

let symetrical p =
  { p with pos = { p.pos with x = world_width -. p.pos.x } }

let symetrical_rect (c1,c2) =
  { c1 with x = world_width -. c1.x }, { c2 with x = world_width -. c2.x }

let rec (-->) i j =
  if i > j
  then []
  else i::((i+1) --> j)

let line_obs p1 p2 rad =
  let v = vector p1 p2 in
  let d = distance p1 p2 in
  let n = int_of_float (d /. (2.*.rad)) in
  let l = 0 --> n in
  List.map (fun i ->
    let c = float i /. float n in
    { pos = { x = c *. v.vx +. p1.x;
              y = c *. v.vy +. p1.y;};
      size = rad } ) l

let left_obstacles = [
  (* Pop-corn dispensers *)
  { x = 0.265; y = world_height }, { x = 0.335; y = world_height -. 0.07 };
  { x = 0.565; y = world_height }, { x = 0.635; y = world_height -. 0.07 };

  (* Starting area *)
  (*{ x = 0.; y = 0.8 }, { x = 0.4; y = 0.778 };
  { x = 0.; y = 1.2 }, { x = 0.4; y = 1.222 };
  { x = 0.; y = 0.8 }, { x = 0.07; y = 1.2 };*)
]

let fixed_obstacles = [
  (* Central spot-light zone *)
  { x = 1.2; y =  0.}, {x = 1.8; y =  0.1};

  (* Stairs *)
  { x = 0.967; y = world_height }, {x = 2.033; y = world_height -. 0.58 };
] @ (List.map symetrical_rect left_obstacles) @ left_obstacles

let test_obstacles =
  [ ({ x = 1.45; y = 0.95 }, { x = 1.55; y = 1.05 }) ]

let initial_fires = []
  (*List.map (fun (x, y, a) -> ({x;y},a))
  [ 0.4, 0.9, 0.;
    2.6, 0.9, 0.;
    0.9, 1.4, (pi/.2.);
    0.9, 0.4, (pi/.2.);
    2.1, 1.4, (pi/.2.);
    2.1, 0.4, (pi/.2.);
    0.018, 1.2, (pi/.2.);
    2.982, 1.2, (pi/.2.);
    1.3, 0.018, 0.;
    1.7, 0.018, 0.; ]*)

let initial_torches = []
  (*List.map (fun (x, y) -> {x;y})
  [ 0.9, 0.9;
    2.1, 0.9; ]*)

let urg_position = { x = 0.095; y = 0. }

let urg_angle_limits = -. (pi/.2.), (pi/.2.)

let urg_min_distance = 0.04

let urg_angles =
  [|-2.356194490192344836998472601408; -2.350058567040802071090865865699;
    -2.343922643889259749272468980053; -2.337786720737716983364862244343;
    -2.331650797586174661546465358697; -2.325514874434631895638858622988;
    -2.319378951283089573820461737341; -2.313243028131546807912855001632;
    -2.307107104980004486094458115986; -2.300971181828461720186851380276;
    -2.294835258676919398368454494630; -2.288699335525376632460847758921;
    -2.282563412373833866553241023212; -2.276427489222291544734844137565;
    -2.270291566070748778827237401856; -2.264155642919206457008840516210;
    -2.258019719767663691101233780500; -2.251883796616121369282836894854;
    -2.245747873464578603375230159145; -2.239611950313036281556833273498;
    -2.233476027161493515649226537789; -2.227340104009951193830829652143;
    -2.221204180858408427923222916434; -2.215068257706865662015616180724;
    -2.208932334555323340197219295078; -2.202796411403780574289612559369;
    -2.196660488252238252471215673722; -2.190524565100695486563608938013;
    -2.184388641949153164745212052367; -2.178252718797610398837605316658;
    -2.172116795646068077019208431011; -2.165980872494525311111601695302;
    -2.159844949342982545203994959593; -2.153709026191440223385598073946;
    -2.147573103039897457477991338237; -2.141437179888355135659594452591;
    -2.135301256736812369751987716882; -2.129165333585270047933590831235;
    -2.123029410433727282025984095526; -2.116893487282184960207587209879;
    -2.110757564130642194299980474170; -2.104621640979099872481583588524;
    -2.098485717827557106573976852815; -2.092349794676014340666370117106;
    -2.086213871524472018847973231459; -2.080077948372929252940366495750;
    -2.073942025221386931121969610103; -2.067806102069844165214362874394;
    -2.061670178918301843395965988748; -2.055534255766759077488359253039;
    -2.049398332615216755669962367392; -2.043262409463673989762355631683;
    -2.037126486312131667943958746037; -2.030990563160588902036352010327;
    -2.024854640009046136128745274618; -2.018718716857503814310348388972;
    -2.012582793705961048402741653263; -2.006446870554418726584344767616;
    -2.000310947402875960676738031907; -1.994175024251333416813736221229;
    -1.988039101099790872950734410551; -1.981903177948248329087732599874;
    -1.975767254796705785224730789196; -1.969631331645163241361728978518;
    -1.963495408493620697498727167840; -1.957359485342078153635725357162;
    -1.951223562190535609772723546484; -1.945087639038993065909721735807;
    -1.938951715887450522046719925129; -1.932815792735907978183718114451;
    -1.926679869584365212276111378742; -1.920543946432822668413109568064;
    -1.914408023281280124550107757386; -1.908272100129737580687105946708;
    -1.902136176978195036824104136031; -1.896000253826652492961102325353;
    -1.889864330675109949098100514675; -1.883728407523567405235098703997;
    -1.877592484372024861372096893319; -1.871456561220482317509095082642;
    -1.865320638068939551601488346932; -1.859184714917397007738486536255;
    -1.853048791765854463875484725577; -1.846912868614311920012482914899;
    -1.840776945462769376149481104221; -1.834641022311226832286479293543;
    -1.828505099159684288423477482866; -1.822369176008141744560475672188;
    -1.816233252856599200697473861510; -1.810097329705056656834472050832;
    -1.803961406553514112971470240154; -1.797825483401971347063863504445;
    -1.791689560250428803200861693767; -1.785553637098886259337859883090;
    -1.779417713947343715474858072412; -1.773281790795801171611856261734;
    -1.767145867644258627748854451056; -1.761009944492716083885852640378;
    -1.754874021341173540022850829700; -1.748738098189630996159849019023;
    -1.742602175038088452296847208345; -1.736466251886545908433845397667;
    -1.730330328735003142526238661958; -1.724194405583460598663236851280;
    -1.718058482431918054800235040602; -1.711922559280375510937233229924;
    -1.705786636128832967074231419247; -1.699650712977290423211229608569;
    -1.693514789825747879348227797891; -1.687378866674205335485225987213;
    -1.681242943522662791622224176535; -1.675107020371120247759222365858;
    -1.668971097219577703896220555180; -1.662835174068034937988613819471;
    -1.656699250916492394125612008793; -1.650563327764949850262610198115;
    -1.644427404613407306399608387437; -1.638291481461864762536606576759;
    -1.632155558310322218673604766082; -1.626019635158779674810602955404;
    -1.619883712007237130947601144726; -1.613747788855694587084599334048;
    -1.607611865704152043221597523370; -1.601475942552609277313990787661;
    -1.595340019401066733450988976983; -1.589204096249524189587987166306;
    -1.583068173097981645724985355628; -1.576932249946439101861983544950;
    -1.570796326794896557998981734272; -1.564660403643354014135979923594;
    -1.558524480491811470272978112916; -1.552388557340268926409976302239;
    -1.546252634188726382546974491561; -1.540116711037183838683972680883;
    -1.533980787885641072776365945174; -1.527844864734098528913364134496;
    -1.521708941582555985050362323818; -1.515573018431013441187360513140;
    -1.509437095279470897324358702463; -1.503301172127928353461356891785;
    -1.497165248976385809598355081107; -1.491029325824843265735353270429;
    -1.484893402673300721872351459751; -1.478757479521758178009349649074;
    -1.472621556370215412101742913364; -1.466485633218672868238741102687;
    -1.460349710067130324375739292009; -1.454213786915587780512737481331;
    -1.448077863764045236649735670653; -1.441941940612502692786733859975;
    -1.435806017460960148923732049298; -1.429670094309417605060730238620;
    -1.423534171157875061197728427942; -1.417398248006332517334726617264;
    -1.411262324854789973471724806586; -1.405126401703247207564118070877;
    -1.398990478551704663701116260199; -1.392854555400162119838114449522;
    -1.386718632248619575975112638844; -1.380582709097077032112110828166;
    -1.374446785945534488249109017488; -1.368310862793991944386107206810;
    -1.362174939642449400523105396132; -1.356039016490906856660103585455;
    -1.349903093339364312797101774777; -1.343767170187821768934099964099;
    -1.337631247036279003026493228390; -1.331495323884736459163491417712;
    -1.325359400733193915300489607034; -1.319223477581651371437487796356;
    -1.313087554430108827574485985679; -1.306951631278566283711484175001;
    -1.300815708127023739848482364323; -1.294679784975481195985480553645;
    -1.288543861823938652122478742967; -1.282407938672396108259476932290;
    -1.276272015520853564396475121612; -1.270136092369310798488868385903;
    -1.264000169217768254625866575225; -1.257864246066225710762864764547;
    -1.251728322914683166899862953869; -1.245592399763140623036861143191;
    -1.239456476611598079173859332514; -1.233320553460055535310857521836;
    -1.227184630308512991447855711158; -1.221048707156970447584853900480;
    -1.214912784005427903721852089802; -1.208776860853885137814245354093;
    -1.202640937702342593951243543415; -1.196505014550800050088241732738;
    -1.190369091399257506225239922060; -1.184233168247714962362238111382;
    -1.178097245096172418499236300704; -1.171961321944629874636234490026;
    -1.165825398793087330773232679348; -1.159689475641544786910230868671;
    -1.153553552490002243047229057993; -1.147417629338459699184227247315;
    -1.141281706186916933276620511606; -1.135145783035374389413618700928;
    -1.129009859883831845550616890250; -1.122873936732289301687615079572;
    -1.116738013580746757824613268895; -1.110602090429204213961611458217;
    -1.104466167277661670098609647539; -1.098330244126119126235607836861;
    -1.092194320974576582372606026183; -1.086058397823034038509604215506;
    -1.079922474671491272601997479796; -1.073786551519948728738995669119;
    -1.067650628368406184875993858441; -1.061514705216863641012992047763;
    -1.055378782065321097149990237085; -1.049242858913778553286988426407;
    -1.043106935762236009423986615730; -1.036971012610693465560984805052;
    -1.030835089459150921697982994374; -1.024699166307608377834981183696;
    -1.018563243156065833971979373018; -1.012427320004523068064372637309;
    -1.006291396852980524201370826631; -1.000155473701437980338369015954;
    -0.994019550549895436475367205276; -0.987883627398352892612365394598;
    -0.981747704246810348749363583920; -0.975611781095267804886361773242;
    -0.969475857943725261023359962564; -0.963339934792182606138055689371;
    -0.957204011640640062275053878693; -0.951068088489097518412052068015;
    -0.944932165337554974549050257338; -0.938796242186012430686048446660;
    -0.932660319034469775800744173466; -0.926524395882927231937742362788;
    -0.920388472731384688074740552111; -0.914252549579842144211738741433;
    -0.908116626428299600348736930755; -0.901980703276757056485735120077;
    -0.895844780125214401600430846884; -0.889708856973671857737429036206;
    -0.883572933822129313874427225528; -0.877437010670586770011425414850;
    -0.871301087519044226148423604172; -0.865165164367501571263119330979;
    -0.859029241215959027400117520301; -0.852893318064416483537115709623;
    -0.846757394912873939674113898946; -0.840621471761331395811112088268;
    -0.834485548609788851948110277590; -0.828349625458246197062806004396;
    -0.822213702306703653199804193719; -0.816077779155161109336802383041;
    -0.809941856003618565473800572363; -0.803805932852076021610798761685;
    -0.797670009700533366725494488492; -0.791534086548990822862492677814;
    -0.785398163397448278999490867136; -0.779262240245905735136489056458;
    -0.773126317094363191273487245780; -0.766990393942820536388182972587;
    -0.760854470791277992525181161909; -0.754718547639735448662179351231;
    -0.748582624488192904799177540554; -0.742446701336650360936175729876;
    -0.736310778185107706050871456682; -0.730174855033565162187869646004;
    -0.724038931882022618324867835327; -0.717903008730480074461866024649;
    -0.711767085578937530598864213971; -0.705631162427394986735862403293;
    -0.699495239275852331850558130100; -0.693359316124309787987556319422;
    -0.687223392972767244124554508744; -0.681087469821224700261552698066;
    -0.674951546669682156398550887388; -0.668815623518139501513246614195;
    -0.662679700366596957650244803517; -0.656543777215054413787242992839;
    -0.650407854063511869924241182161; -0.644271930911969326061239371484;
    -0.638136007760426782198237560806; -0.632000084608884127312933287612;
    -0.625864161457341583449931476935; -0.619728238305799039586929666257;
    -0.613592315154256495723927855579; -0.607456392002713951860926044901;
    -0.601320468851171296975621771708; -0.595184545699628753112619961030;
    -0.589048622548086209249618150352; -0.582912699396543665386616339674;
    -0.576776776245001121523614528996; -0.570640853093458466638310255803;
    -0.564504929941915922775308445125; -0.558369006790373378912306634447;
    -0.552233083638830835049304823769; -0.546097160487288291186303013092;
    -0.539961237335745636300998739898; -0.533825314184203092437996929220;
    -0.527689391032660548574995118543; -0.521553467881118004711993307865;
    -0.515417544729575460848991497187; -0.509281621578032916985989686509;
    -0.503145698426490262100685413316; -0.497009775274947718237683602638;
    -0.490873852123405174374681791960; -0.484737928971862630511679981282;
    -0.478602005820320031137526939347; -0.472466082668777487274525128669;
    -0.466330159517234887900372086733; -0.460194236365692344037370276055;
    -0.454058313214149800174368465377; -0.447922390062607200800215423442;
    -0.441786466911064656937213612764; -0.435650543759522113074211802086;
    -0.429514620607979513700058760151; -0.423378697456436969837056949473;
    -0.417242774304894425974055138795; -0.411106851153351826599902096859;
    -0.404970928001809282736900286181; -0.398835004850266683362747244246;
    -0.392699081698724139499745433568; -0.386563158547181595636743622890;
    -0.380427235395638996262590580955; -0.374291312244096452399588770277;
    -0.368155389092553853025435728341; -0.362019465941011309162433917663;
    -0.355883542789468765299432106985; -0.349747619637926165925279065050;
    -0.343611696486383622062277254372; -0.337475773334841078199275443694;
    -0.331339850183298478825122401759; -0.325203927031755934962120591081;
    -0.319068003880213391099118780403; -0.312932080728670791724965738467;
    -0.306796157577128247861963927789; -0.300660234425585648487810885854;
    -0.294524311274043104624809075176; -0.288388388122500560761807264498;
    -0.282252464970957961387654222563; -0.276116541819415417524652411885;
    -0.269980618667872818150499369949; -0.263844695516330274287497559271;
    -0.257708772364787730424495748593; -0.251572849213245131050342706658;
    -0.245436926061702587187340895980; -0.239301002910160015568763469673;
    -0.233165079758617443950186043367; -0.227029156607074900087184232689;
    -0.220893233455532328468606806382; -0.214757310303989756850029380075;
    -0.208621387152447212987027569397; -0.202485464000904641368450143091;
    -0.196349540849362069749872716784; -0.190213617697819498131295290477;
    -0.184077694546276926512717864171; -0.177941771394734382649716053493;
    -0.171805848243191811031138627186; -0.165669925091649239412561200879;
    -0.159534001940106695549559390201; -0.153398078788564123930981963895;
    -0.147262155637021552312404537588; -0.141126232485478980693827111281;
    -0.134990309333936409075249684975; -0.128854386182393865212247874297;
    -0.122718463030851293593670447990; -0.116582539879308721975093021683;
    -0.110446616727766164234303403191; -0.104310693576223606493513784699;
    -0.098174770424681034874936358392; -0.092038847273138463256358932085;
    -0.085902924121595905515569313593; -0.079767000970053347774779695101;
    -0.073631077818510776156202268794; -0.067495154666968204537624842487;
    -0.061359231515425646796835223995; -0.055223308363883082117151701596;
    -0.049087385212340517437468179196; -0.042951462060797952757784656797;
    -0.036815538909255388078101134397; -0.030679615757712823398417611998;
    -0.024543692606170258718734089598; -0.018407769454627694039050567199;
    -0.012271846303085129359367044799; -0.006135923151542564679683522400;
    0.000000000000000000000000000000; 0.006135923151542564679683522400;
    0.012271846303085129359367044799; 0.018407769454627694039050567199;
    0.024543692606170258718734089598; 0.030679615757712823398417611998;
    0.036815538909255388078101134397; 0.042951462060797952757784656797;
    0.049087385212340517437468179196; 0.055223308363883082117151701596;
    0.061359231515425646796835223995; 0.067495154666968204537624842487;
    0.073631077818510776156202268794; 0.079767000970053347774779695101;
    0.085902924121595905515569313593; 0.092038847273138463256358932085;
    0.098174770424681034874936358392; 0.104310693576223606493513784699;
    0.110446616727766164234303403191; 0.116582539879308721975093021683;
    0.122718463030851293593670447990; 0.128854386182393865212247874297;
    0.134990309333936409075249684975; 0.141126232485478980693827111281;
    0.147262155637021552312404537588; 0.153398078788564123930981963895;
    0.159534001940106695549559390201; 0.165669925091649239412561200879;
    0.171805848243191811031138627186; 0.177941771394734382649716053493;
    0.184077694546276926512717864171; 0.190213617697819498131295290477;
    0.196349540849362069749872716784; 0.202485464000904641368450143091;
    0.208621387152447212987027569397; 0.214757310303989756850029380075;
    0.220893233455532328468606806382; 0.227029156607074900087184232689;
    0.233165079758617443950186043367; 0.239301002910160015568763469673;
    0.245436926061702587187340895980; 0.251572849213245131050342706658;
    0.257708772364787730424495748593; 0.263844695516330274287497559271;
    0.269980618667872818150499369949; 0.276116541819415417524652411885;
    0.282252464970957961387654222563; 0.288388388122500560761807264498;
    0.294524311274043104624809075176; 0.300660234425585648487810885854;
    0.306796157577128247861963927789; 0.312932080728670791724965738467;
    0.319068003880213391099118780403; 0.325203927031755934962120591081;
    0.331339850183298478825122401759; 0.337475773334841078199275443694;
    0.343611696486383622062277254372; 0.349747619637926165925279065050;
    0.355883542789468765299432106985; 0.362019465941011309162433917663;
    0.368155389092553853025435728341; 0.374291312244096452399588770277;
    0.380427235395638996262590580955; 0.386563158547181595636743622890;
    0.392699081698724139499745433568; 0.398835004850266683362747244246;
    0.404970928001809282736900286181; 0.411106851153351826599902096859;
    0.417242774304894425974055138795; 0.423378697456436969837056949473;
    0.429514620607979513700058760151; 0.435650543759522113074211802086;
    0.441786466911064656937213612764; 0.447922390062607200800215423442;
    0.454058313214149800174368465377; 0.460194236365692344037370276055;
    0.466330159517234887900372086733; 0.472466082668777487274525128669;
    0.478602005820320031137526939347; 0.484737928971862630511679981282;
    0.490873852123405174374681791960; 0.497009775274947718237683602638;
    0.503145698426490262100685413316; 0.509281621578032916985989686509;
    0.515417544729575460848991497187; 0.521553467881118004711993307865;
    0.527689391032660548574995118543; 0.533825314184203092437996929220;
    0.539961237335745636300998739898; 0.546097160487288291186303013092;
    0.552233083638830835049304823769; 0.558369006790373378912306634447;
    0.564504929941915922775308445125; 0.570640853093458466638310255803;
    0.576776776245001121523614528996; 0.582912699396543665386616339674;
    0.589048622548086209249618150352; 0.595184545699628753112619961030;
    0.601320468851171296975621771708; 0.607456392002713951860926044901;
    0.613592315154256495723927855579; 0.619728238305799039586929666257;
    0.625864161457341583449931476935; 0.632000084608884127312933287612;
    0.638136007760426782198237560806; 0.644271930911969326061239371484;
    0.650407854063511869924241182161; 0.656543777215054413787242992839;
    0.662679700366596957650244803517; 0.668815623518139501513246614195;
    0.674951546669682156398550887388; 0.681087469821224700261552698066;
    0.687223392972767244124554508744; 0.693359316124309787987556319422;
    0.699495239275852331850558130100; 0.705631162427394986735862403293;
    0.711767085578937530598864213971; 0.717903008730480074461866024649;
    0.724038931882022618324867835327; 0.730174855033565162187869646004;
    0.736310778185107706050871456682; 0.742446701336650360936175729876;
    0.748582624488192904799177540554; 0.754718547639735448662179351231;
    0.760854470791277992525181161909; 0.766990393942820536388182972587;
    0.773126317094363191273487245780; 0.779262240245905735136489056458;
    0.785398163397448278999490867136; 0.791534086548990822862492677814;
    0.797670009700533366725494488492; 0.803805932852076021610798761685;
    0.809941856003618565473800572363; 0.816077779155161109336802383041;
    0.822213702306703653199804193719; 0.828349625458246197062806004396;
    0.834485548609788851948110277590; 0.840621471761331395811112088268;
    0.846757394912873939674113898946; 0.852893318064416483537115709623;
    0.859029241215959027400117520301; 0.865165164367501571263119330979;
    0.871301087519044226148423604172; 0.877437010670586770011425414850;
    0.883572933822129313874427225528; 0.889708856973671857737429036206;
    0.895844780125214401600430846884; 0.901980703276757056485735120077;
    0.908116626428299600348736930755; 0.914252549579842144211738741433;
    0.920388472731384688074740552111; 0.926524395882927231937742362788;
    0.932660319034469775800744173466; 0.938796242186012430686048446660;
    0.944932165337554974549050257338; 0.951068088489097518412052068015;
    0.957204011640640062275053878693; 0.963339934792182606138055689371;
    0.969475857943725261023359962564; 0.975611781095267804886361773242;
    0.981747704246810348749363583920; 0.987883627398352892612365394598;
    0.994019550549895436475367205276; 1.000155473701437980338369015954;
    1.006291396852980524201370826631; 1.012427320004523068064372637309;
    1.018563243156065833971979373018; 1.024699166307608377834981183696;
    1.030835089459150921697982994374; 1.036971012610693465560984805052;
    1.043106935762236009423986615730; 1.049242858913778553286988426407;
    1.055378782065321097149990237085; 1.061514705216863641012992047763;
    1.067650628368406184875993858441; 1.073786551519948728738995669119;
    1.079922474671491272601997479796; 1.086058397823034038509604215506;
    1.092194320974576582372606026183; 1.098330244126119126235607836861;
    1.104466167277661670098609647539; 1.110602090429204213961611458217;
    1.116738013580746757824613268895; 1.122873936732289301687615079572;
    1.129009859883831845550616890250; 1.135145783035374389413618700928;
    1.141281706186916933276620511606; 1.147417629338459699184227247315;
    1.153553552490002243047229057993; 1.159689475641544786910230868671;
    1.165825398793087330773232679348; 1.171961321944629874636234490026;
    1.178097245096172418499236300704; 1.184233168247714962362238111382;
    1.190369091399257506225239922060; 1.196505014550800050088241732738;
    1.202640937702342593951243543415; 1.208776860853885137814245354093;
    1.214912784005427903721852089802; 1.221048707156970447584853900480;
    1.227184630308512991447855711158; 1.233320553460055535310857521836;
    1.239456476611598079173859332514; 1.245592399763140623036861143191;
    1.251728322914683166899862953869; 1.257864246066225710762864764547;
    1.264000169217768254625866575225; 1.270136092369310798488868385903;
    1.276272015520853564396475121612; 1.282407938672396108259476932290;
    1.288543861823938652122478742967; 1.294679784975481195985480553645;
    1.300815708127023739848482364323; 1.306951631278566283711484175001;
    1.313087554430108827574485985679; 1.319223477581651371437487796356;
    1.325359400733193915300489607034; 1.331495323884736459163491417712;
    1.337631247036279003026493228390; 1.343767170187821768934099964099;
    1.349903093339364312797101774777; 1.356039016490906856660103585455;
    1.362174939642449400523105396132; 1.368310862793991944386107206810;
    1.374446785945534488249109017488; 1.380582709097077032112110828166;
    1.386718632248619575975112638844; 1.392854555400162119838114449522;
    1.398990478551704663701116260199; 1.405126401703247207564118070877;
    1.411262324854789973471724806586; 1.417398248006332517334726617264;
    1.423534171157875061197728427942; 1.429670094309417605060730238620;
    1.435806017460960148923732049298; 1.441941940612502692786733859975;
    1.448077863764045236649735670653; 1.454213786915587780512737481331;
    1.460349710067130324375739292009; 1.466485633218672868238741102687;
    1.472621556370215412101742913364; 1.478757479521758178009349649074;
    1.484893402673300721872351459751; 1.491029325824843265735353270429;
    1.497165248976385809598355081107; 1.503301172127928353461356891785;
    1.509437095279470897324358702463; 1.515573018431013441187360513140;
    1.521708941582555985050362323818; 1.527844864734098528913364134496;
    1.533980787885641072776365945174; 1.540116711037183838683972680883;
    1.546252634188726382546974491561; 1.552388557340268926409976302239;
    1.558524480491811470272978112916; 1.564660403643354014135979923594;
    1.570796326794896557998981734272; 1.576932249946439101861983544950;
    1.583068173097981645724985355628; 1.589204096249524189587987166306;
    1.595340019401066733450988976983; 1.601475942552609277313990787661;
    1.607611865704152043221597523370; 1.613747788855694587084599334048;
    1.619883712007237130947601144726; 1.626019635158779674810602955404;
    1.632155558310322218673604766082; 1.638291481461864762536606576759;
    1.644427404613407306399608387437; 1.650563327764949850262610198115;
    1.656699250916492394125612008793; 1.662835174068034937988613819471;
    1.668971097219577703896220555180; 1.675107020371120247759222365858;
    1.681242943522662791622224176535; 1.687378866674205335485225987213;
    1.693514789825747879348227797891; 1.699650712977290423211229608569;
    1.705786636128832967074231419247; 1.711922559280375510937233229924;
    1.718058482431918054800235040602; 1.724194405583460598663236851280;
    1.730330328735003142526238661958; 1.736466251886545908433845397667;
    1.742602175038088452296847208345; 1.748738098189630996159849019023;
    1.754874021341173540022850829700; 1.761009944492716083885852640378;
    1.767145867644258627748854451056; 1.773281790795801171611856261734;
    1.779417713947343715474858072412; 1.785553637098886259337859883090;
    1.791689560250428803200861693767; 1.797825483401971347063863504445;
    1.803961406553514112971470240154; 1.810097329705056656834472050832;
    1.816233252856599200697473861510; 1.822369176008141744560475672188;
    1.828505099159684288423477482866; 1.834641022311226832286479293543;
    1.840776945462769376149481104221; 1.846912868614311920012482914899;
    1.853048791765854463875484725577; 1.859184714917397007738486536255;
    1.865320638068939551601488346932; 1.871456561220482317509095082642;
    1.877592484372024861372096893319; 1.883728407523567405235098703997;
    1.889864330675109949098100514675; 1.896000253826652492961102325353;
    1.902136176978195036824104136031; 1.908272100129737580687105946708;
    1.914408023281280124550107757386; 1.920543946432822668413109568064;
    1.926679869584365212276111378742; 1.932815792735907978183718114451;
    1.938951715887450522046719925129; 1.945087639038993065909721735807;
    1.951223562190535609772723546484; 1.957359485342078153635725357162;
    1.963495408493620697498727167840; 1.969631331645163241361728978518;
    1.975767254796705785224730789196; 1.981903177948248329087732599874;
    1.988039101099790872950734410551; 1.994175024251333416813736221229;
    2.000310947402875960676738031907; 2.006446870554418726584344767616;
    2.012582793705961048402741653263; 2.018718716857503814310348388972;
    2.024854640009046136128745274618; 2.030990563160588902036352010327;
    2.037126486312131667943958746037; 2.043262409463673989762355631683;
    2.049398332615216755669962367392; 2.055534255766759077488359253039;
    2.061670178918301843395965988748; 2.067806102069844165214362874394;
    2.073942025221386931121969610103; 2.080077948372929252940366495750;
    2.086213871524472018847973231459; 2.092349794676014340666370117106; |]

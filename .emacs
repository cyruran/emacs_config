(add-to-list 'load-path "~/emacs_config/")
(require 'common)

;; Customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(beacon-color "#ec4780")
 '(column-number-mode t)
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (arjen-grey)))
 '(custom-safe-themes
   (quote
    ("01ce486c3a7c8b37cf13f8c95ca4bb3c11413228b35676025fdf239e77019ea1" "63dd8ce36f352b92dbf4f80e912ac68216c1d7cf6ae98195e287fd7c7f7cb189" "23ccf46b0d05ae80ee0661b91a083427a6c61e7a260227d37e36833d862ccffc" "b9b1a8d2ec1d5c17700e1a09256f33c2520b26f49980ed9e217e444c381279a9" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "eb07ee737bae7860ff12a4dbd2dcb9ff9712e517cfd6279fa74f04a17b6e1ba6" "756ec68798410a2e705dd719c7328af9ecbb782c94130d489b6b3109841833eb" "4bf9b00abab609ecc2a405aa25cc5e1fb5829102cf13f05af6a7831d968c59de" "8be07a2c1b3a7300860c7a65c0ad148be6d127671be04d3d2120f1ac541ac103" "722e1cd0dad601ec6567c32520126e42a8031cd72e05d2221ff511b58545b108" "4ae980547881835ffc0cb858ed7547c64f923398f344e0083cadd28fa2e59300" "60e09d2e58343186a59d9ed52a9b13d822a174b33f20bdc1d4abb86e6b17f45b" "aded4ec996e438a5e002439d58f09610b330bbc18f580c83ebaba026bbef6c82" "0c3b1358ea01895e56d1c0193f72559449462e5952bded28c81a8e09b53f103f" "ef04dd1e33f7cbd5aa3187981b18652b8d5ac9e680997b45dc5d00443e6a46e3" "87d46d0ad89557c616d04bef34afd191234992c4eb955ff3c60c6aa3afc2e5cc" "986e7e8e428decd5df9e8548a3f3b42afc8176ce6171e69658ae083f3c06211c" "f869a5d068a371532c82027cdf1feefdc5768757c78c48a7e0177e90651503ad" "eae831de756bb480240479794e85f1da0789c6f2f7746e5cc999370bbc8d9c8a" "4bf5c18667c48f2979ead0f0bdaaa12c2b52014a6abaa38558a207a65caeb8ad" "5a7830712d709a4fc128a7998b7fa963f37e960fd2e8aa75c76f692b36e6cf3c" "c968804189e0fc963c641f5c9ad64bca431d41af2fb7e1d01a2a6666376f819c" "d9850d120be9d94dd7ae69053630e89af8767c36b131a3aa7b06f14007a24656" "d9dab332207600e49400d798ed05f38372ec32132b3f7d2ba697e59088021555" "250268d5c0b4877cc2b7c439687f8145a2c85a48981f7070a72c7f47a2d2dc13" "d96587ec2c7bf278269b8ec2b800c7d9af9e22d816827639b332b0e613314dfd" "5b8eccff13d79fc9b26c544ee20e1b0c499587d6c4bfc38cabe34beaf2c2fc77" "3f67aee8f8d8eedad7f547a346803be4cc47c420602e19d88bdcccc66dba033b" "8543b328ed10bc7c16a8a35c523699befac0de00753824d7e90148bca583f986" "fec45178b55ad0258c5f68f61c9c8fd1a47d73b08fb7a51c15558d42c376083d" "6271fc9740379f8e2722f1510d481c1df1fcc43e48fa6641a5c19e954c21cc8f" "ad16a1bf1fd86bfbedae4b32c269b19f8d20d416bd52a87cd50e355bf13c2f23" "d83e34e28680f2ed99fe50fea79f441ca3fddd90167a72b796455e791c90dc49" "100eeb65d336e3d8f419c0f09170f9fd30f688849c5e60a801a1e6addd8216cb" "fee4e306d9070a55dce4d8e9d92d28bd9efe92625d2ba9d4d654fc9cd8113b7f" "50d07ab55e2b5322b2a8b13bc15ddf76d7f5985268833762c500a90e2a09e7aa" "4feee83c4fbbe8b827650d0f9af4ba7da903a5d117d849a3ccee88262805f40d" "6daa09c8c2c68de3ff1b83694115231faa7e650fdbb668bc76275f0f2ce2a437" "4a91a64af7ff1182ed04f7453bb5a4b0c3d82148d27db699df89a5f1d449e2a4" "73ad471d5ae9355a7fa28675014ae45a0589c14492f52c32a4e9b393fcc333fd" "b8929cff63ffc759e436b0f0575d15a8ad7658932f4b2c99415f3dde09b32e97" "9c4acf7b5801f25501f0db26ac3eee3dc263ed51afd01f9dcfda706a15234733" "85d609b07346d3220e7da1e0b87f66d11b2eeddad945cac775e80d2c1adb0066" "a62f0662e6aa7b05d0b4493a8e245ab31492765561b08192df61c9d1c7e1ddee" "04790c9929eacf32d508b84d34e80ad2ee233f13f17767190531b8b350b9ef22" "e04cdda50908b116031c09d7b316fff5d8f9bc6e2126411c9316969461bfd8b6" "f5f3a6fb685fe5e1587bafd07db3bf25a0655f3ddc579ed9d331b6b19827ea46" "9955cc54cc64d6c051616dce7050c1ba34efc2b0613d89a70a68328f34e22c8f" "542e6fee85eea8e47243a5647358c344111aa9c04510394720a3108803c8ddd1" "159aab698b9d3fb03b495ce3af2d298f4c6dfdf21b53c27cd7f472ee5a1a1de3" "ffac21ab88a0f4603969a24b96993bd73a13fe0989db7ed76d94c305891fad64" "8578750fb94f908249a98dc14c3847d11863196f54de87a037b1374f2ae1f534" "45a8b89e995faa5c69aa79920acff5d7cb14978fbf140cdd53621b09d782edcf" "5228973368d5a1ac0cbea0564d0cd724937f52cc06a8fd81fc65a4fa72ff837b" "41eb3fe4c6b80c7ad156a8c52e9dd6093e8856c7bbf2b92cc3a4108ceb385087" "5f99055206ed6a1b9958f7dd5eaa9f884f8b5a8678bd0c5e2622aced5c4a1be7" "fc7fd2530b82a722ceb5b211f9e732d15ad41d5306c011253a0ba43aaf93dccc" "52741e091463c2217af9327e2b2d74d0df861ecc3ad6131b6cbcb8d76b7a4d3d" "3e34e9bf818cf6301fcabae2005bba8e61b1caba97d95509c8da78cff5f2ec8e" "ccde32eaf485eb7579412cd756d10b0f20f89bff07696972d7ee46cb2e10b89d" "cabc32838ccceea97404f6fcb7ce791c6e38491fd19baa0fcfb336dcc5f6e23c" "4605ce6e798971d215b01844ea39e993d683aa2fa118e02e263539298f9f3921" "1d079355c721b517fdc9891f0fda927fe3f87288f2e6cc3b8566655a64ca5453" "34ed3e2fa4a1cb2ce7400c7f1a6c8f12931d8021435bad841fdc1192bd1cc7da" "b3bcf1b12ef2a7606c7697d71b934ca0bdd495d52f901e73ce008c4c9825a3aa" "62408b3adcd05f887b6357e5bd9221652984a389e9b015f87bbc596aba62ba48" "12670281275ea7c1b42d0a548a584e23b9c4e1d2dabb747fd5e2d692bcd0d39b" "36746ad57649893434c443567cb3831828df33232a7790d232df6f5908263692" "1263771faf6967879c3ab8b577c6c31020222ac6d3bac31f331a74275385a452" "92192ea8f0bf04421f5b245d906701abaa7bb3b0d2b3b14fca2ee5ebb1da38d8" "dd6e52a5b1180f5c8bf408764a32867e2fa86594ded78a29040cafce6a4ea808" "ca8e634fa3e088ef5e19a6e609f9e79fd407a6188fcb5bc3de17801ed38f8afa" "4d98eb21cd7aacc0e7e5db895dba4c99a51b74c0a56eea63060a87f87b807461" "3e335d794ed3030fefd0dbd7ff2d3555e29481fe4bbb0106ea11c660d6001767" "cc0dbb53a10215b696d391a90de635ba1699072745bf653b53774706999208e3" "85e6bb2425cbfeed2f2b367246ad11a62fb0f6d525c157038a0d0eaaabc1bfee" "78c1c89192e172436dbf892bd90562bc89e2cc3811b5f9506226e735a953a9c6" "6145e62774a589c074a31a05dfa5efdf8789cf869104e905956f0cbd7eda9d0e" "ffe80c88e3129b2cddadaaf78263a7f896d833a77c96349052ad5b7753c0c5a5" "cbd8e65d2452dfaed789f79c92d230aa8bdf413601b261dbb1291fb88605110c" "df21cdadd3f0648e3106338649d9fea510121807c907e2fd15565dde6409d6e9" "3de3f36a398d2c8a4796360bfce1fa515292e9f76b655bb9a377289a6a80a132" "a85e40c7d2df4a5e993742929dfd903899b66a667547f740872797198778d7b5" "9be1d34d961a40d94ef94d0d08a364c3d27201f3c98c9d38e36f10588469ea57" "2a998a3b66a0a6068bcb8b53cd3b519d230dd1527b07232e54c8b9d84061d48d" "16dd114a84d0aeccc5ad6fd64752a11ea2e841e3853234f19dc02a7b91f5d661" "dd4628d6c2d1f84ad7908c859797b24cc6239dfe7d71b3363ccdd2b88963f336" "840db7f67ce92c39deb38f38fbc5a990b8f89b0f47b77b96d98e4bf400ee590a" "5a39d2a29906ab273f7900a2ae843e9aa29ed5d205873e1199af4c9ec921aaab" "7bef2d39bac784626f1635bd83693fae091f04ccac6b362e0405abf16a32230c" "cea3ec09c821b7eaf235882e6555c3ffa2fd23de92459751e18f26ad035d2142" "3f546f127564de59f1c42dfd65d32bd7650fa5a614cd58c2df061da052ea3ec0" "427fed191e7a766152e59ef0e2904283f436dbbe259b9ccc04989f3acde50a55" "1606c3a5e58d74a10289df3c7a4005b670e2b80a54c87f05263862cbe4626ac5" "25c242b3c808f38b0389879b9cba325fb1fa81a0a5e61ac7cae8da9a32e2811b" "760ce657e710a77bcf6df51d97e51aae2ee7db1fba21bbad07aab0fa0f42f834" "6f3e8df833cdaaef4ae0fb5cad70f5f74afc834a5eb8eec45efa5dd8f7356f9d" "e1498b2416922aa561076edc5c9b0ad7b34d8ff849f335c13364c8f4276904f0" "93268bf5365f22c685550a3cbb8c687a1211e827edc76ce7be3c4bd764054bad" "aea30125ef2e48831f46695418677b9d676c3babf43959c8e978c0ad672a7329" "8cf1002c7f805360115700144c0031b9cfa4d03edc6a0f38718cef7b7cabe382" "c11421683c971b41d154b1a4ef20a2c800537b72fefa618b50b184bbfe6b48a0" "aeb698d431751b18153e89b5f838fc3992433780a39a082740db216c7202a1c9" "3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" "4d80487632a0a5a72737a7fc690f1f30266668211b17ba836602a8da890c2118" "d8f76414f8f2dcb045a37eb155bfaa2e1d17b6573ed43fb1d18b936febc7bbc2" "40bc0ac47a9bd5b8db7304f8ef628d71e2798135935eb450483db0dbbfff8b11" "603a9c7f3ca3253cb68584cb26c408afcf4e674d7db86badcfe649dd3c538656" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "c5a886cc9044d8e6690a60f33db45506221aa0777a82ad1f7fe11a96d203fa44" "cf35cc71022eb3744a6e48934e39d817612d64893018c86373d1482efdc2f944" "790e74b900c074ac8f64fa0b610ad05bcfece9be44e8f5340d2d94c1e47538de" "5cd0afd0ca01648e1fff95a7a7f8abec925bd654915153fb39ee8e72a8b56a1f" "6f441c0e5d8199f08eb4b73e9c697710282bcae95e5925b7649ddfa8cea2e24c" "97d039a52cfb190f4fd677f02f7d03cf7dbd353e08ac8a0cb991223b135ac4e6" "0dfa1f356bdb48aa03088d4034b90c65290eb4373565f52f629fdee0af92a444" "78559045fb299f3542c232166ad635c59cf0c6578d80a58b885deafe98a36c66" "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" "70f5a47eb08fe7a4ccb88e2550d377ce085fedce81cf30c56e3077f95a2909f2" "c3e6b52caa77cb09c049d3c973798bc64b5c43cc437d449eacf35b3e776bf85c" "cc210a8d0cc72968e7c8516c9c7bd5043cc47199755abc5c23cb295a6e715d35" "6af55f6f26c0c6f113427d8ce72dea34aa1972b70e650486e6c725abd18bbe91" "737d9d0e0f6c4279e80f7479ec5138af6e4908a2d052126f254e1e6d1a0d0188" "db2ecce0600e3a5453532a89fc19b139664b4a3e7cbefce3aaf42b6d9b1d6214" "7153b82e50b6f7452b4519097f880d968a6eaf6f6ef38cc45a144958e553fbc6" "5e3fc08bcadce4c6785fc49be686a4a82a356db569f55d411258984e952f194a" "a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" "ab04c00a7e48ad784b52f34aa6bfa1e80d0c3fcacc50e1189af3651013eb0d58" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "a1289424bbc0e9f9877aa2c9a03c7dfd2835ea51d8781a0bf9e2415101f70a7e" "1177fe4645eb8db34ee151ce45518e47cc4595c3e72c55dc07df03ab353ad132" "7356632cebc6a11a87bc5fcffaa49bae528026a78637acd03cae57c091afd9b9" "639d3dd481c17a36f6bcc257e452836ea84cea0c8ba1b31f65320e0215cb98c8" "d606ac41cdd7054841941455c0151c54f8bff7e4e050255dbd4ae4d60ab640c1" "f9574c9ede3f64d57b3aa9b9cef621d54e2e503f4d75d8613cbcc4ca1c962c21" "28ec8ccf6190f6a73812df9bc91df54ce1d6132f18b4c8fcc85d45298569eb53" "5e2dc1360a92bb73dafa11c46ba0f30fa5f49df887a8ede4e3533c3ab6270e08" "8ee6000135468b8fc9343f2ed364ee492b0dd9ac6fee294be9967e3ed32a9f0d" "045251e7ff119a8b065b4cb0072067eb2f297acc44a9e36407e6ff165e35c528" "55d31108a7dc4a268a1432cd60a7558824223684afecefa6fae327212c40f8d3" "dba244449b15bdc6a3236f45cec7c2cb03de0f5cf5709a01158a278da86cb69b" "c1390663960169cd92f58aad44ba3253227d8f715c026438303c09b9fb66cdfb" "95db78d85e3c0e735da28af774dfa59308db832f84b8a2287586f5b4f21a7a5b" "f680d237c4c3252876fbfe1561265ff97a5d3ce67fdc5f6c092e33d476ec0993" default)))
 '(diary-entry-marker (quote font-lock-variable-name-face))
 '(dired-dwim-target t)
 '(dired-recursive-copies (quote always))
 '(dired-recursive-deletes (quote always))
 '(emms-mode-line-icon-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
static char *note[] = {
/* width height num_colors chars_per_pixel */
\"    10   11        2            1\",
/* colors */
\". c #1ba1a1\",
\"# c None s None\",
/* pixels */
\"###...####\",
\"###.#...##\",
\"###.###...\",
\"###.#####.\",
\"###.#####.\",
\"#...#####.\",
\"....#####.\",
\"#..######.\",
\"#######...\",
\"######....\",
\"#######..#\" };")))
 '(evil-emacs-state-cursor (quote ("#E57373" bar)) t)
 '(evil-insert-state-cursor (quote ("#E57373" hbar)) t)
 '(evil-normal-state-cursor (quote ("#FFEE58" box)) t)
 '(evil-visual-state-cursor (quote ("#C5E1A5" box)) t)
 '(eww-search-prefix "https://m.ya.ru/?q=")
 '(fci-rule-character-color "#192028")
 '(fci-rule-color "#192028" t)
 '(font-use-system-font t)
 '(fringe-mode 10 nil (fringe))
 '(gnus-logo-colors (quote ("#1ec1c4" "#bababa")) t)
 '(gnus-mode-line-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    13        2            1\",
/* colors */
\". c #1ba1a1\",
\"# c None s None\",
/* pixels */
\"##################\",
\"######..##..######\",
\"#####........#####\",
\"#.##.##..##...####\",
\"#...####.###...##.\",
\"#..###.######.....\",
\"#####.########...#\",
\"###########.######\",
\"####.###.#..######\",
\"######..###.######\",
\"###....####.######\",
\"###..######.######\",
\"###########.######\" };")) t)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-indent-guides-auto-enabled nil)
 '(highlight-symbol-colors
   (quote
    ("#FFEE58" "#C5E1A5" "#80DEEA" "#64B5F6" "#E1BEE7" "#FFCC80")))
 '(highlight-symbol-foreground-color "#E0E0E0")
 '(highlight-tail-colors (quote (("#ec4780" . 0) ("#424242" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(hl-paren-colors
   (quote
    ("#B9F" "#B8D" "#B7B" "#B69" "#B57" "#B45" "#B33" "#B11")))
 '(linum-format " %6d ")
 '(magit-diff-use-overlays nil)
 '(main-line-color1 "#222912")
 '(main-line-color2 "#09150F")
 '(menu-bar-mode nil)
 '(minimap-mode nil)
 '(mode-icons-mode t)
 '(mode-line-format
   (quote
    ("%e" mode-line-modified mode-line-remote " " mode-line-buffer-identification " "
     ((which-func-mode
       ("" which-func-format " ")))
     ("%l:%c %p")
     " " mode-line-modes
     (vc-mode vc-mode)
     mode-line-end-spaces)))
 '(nrepl-message-colors
   (quote
    ("#336c6c" "#205070" "#0f2050" "#806080" "#401440" "#6c1f1c" "#6b400c" "#23733c")))
 '(org-agenda-files (quote ("/tmp/example.org")))
 '(org-export-with-sub-superscripts (quote {}))
 '(pdf-view-midnight-colors (quote ("#232333" . "#c7c7c7")))
 '(pos-tip-background-color "#3a3a3a")
 '(pos-tip-foreground-color "#9E9E9E")
 '(powerline-color1 "#222912")
 '(powerline-color2 "#09150F")
 '(red "#ffffff")
 '(show-paren-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(tabbar-background-color "#353535")
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#d54e53")
     (40 . "goldenrod")
     (60 . "#e7c547")
     (80 . "DarkOliveGreen3")
     (100 . "#70c0b1")
     (120 . "DeepSkyBlue1")
     (140 . "#c397d8")
     (160 . "#d54e53")
     (180 . "goldenrod")
     (200 . "#e7c547")
     (220 . "DarkOliveGreen3")
     (240 . "#70c0b1")
     (260 . "DeepSkyBlue1")
     (280 . "#c397d8")
     (300 . "#d54e53")
     (320 . "goldenrod")
     (340 . "#e7c547")
     (360 . "DarkOliveGreen3"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"])
 '(yas-global-mode t nil (yasnippet)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 113 :width normal))))
 '(company-preview ((t (:background "DodgerBlue4" :foreground "wheat"))))
 '(company-preview-common ((t (:inherit company-preview :foreground "lavender blush"))))
 '(company-preview-search ((t (:inherit company-preview :background "SlateBlue1"))))
 '(cperl-array-face ((t (:foreground "peru" :weight bold))))
 '(cperl-hash-face ((t (:foreground "indian red" :slant italic :weight bold))))
 '(cperl-nonoverridable-face ((t (:foreground "dark olive green"))))
 '(cursor ((t (:background "gray100"))))
 '(font-lock-warning-face ((t (:foreground "orange red" :weight bold))))
 '(fsvn-link-face ((t (:foreground "deep sky blue" :underline "deep sky blue"))))
 '(link ((t (:foreground "rosy brown" :underline t))))
 '(magit-reflog-checkout ((t (:foreground "deep sky blue"))))
 '(mode-line ((t (:background "black" :foreground "#bdc3ce"))))
 '(powerline-inactive1 ((t (:inherit mode-line-inactive :background "grey11" :foreground "SkyBlue4"))))
 '(ruler-mode-pad ((t (:inherit ruler-mode-default :background "dark gray"))))
 '(which-func ((t (:foreground "linen")))))
(put 'dired-find-alternate-file 'disabled nil)

;; (require 'graphene)

(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

;; Term and Shell
(defface term-color-black 
  '((t (:foreground "#3f3f3f" :background "#272822"))) 
  "Unhelpful docstring.")
(defface term-color-red
  '((t (:foreground "#cc9393" :background "#272822"))) 
  "Unhelpful docstring.")
(defface term-color-green
  '((t (:foreground "#7f9f7f" :background "#272822"))) 
  "Unhelpful docstring.")
(defface term-color-yellow
  '((t (:foreground "#f0dfaf" :background "#272822"))) 
  "Unhelpful docstring.")
(defface term-color-blue 
  '((t (:foreground "#6d85ba" :background "#272822"))) 
  "Unhelpful docstring.")
(defface term-color-magenta 
  '((t (:foreground "#dc8cc3" :background "#272822"))) 
  "Unhelpful docstring.")
(defface term-color-cyan
  '((t (:foreground "#93e0e3" :background "#272822"))) 
  "Unhelpful docstring.")
(defface term-color-white
  '((t (:foreground "#dcdccc" :background "#272822"))) 
  "Unhelpful docstring.")
'(term-default-fg-color ((t (:inherit term-color-white))))
'(term-default-bg-color ((t (:inherit term-color-black))))

;; ansi-term colors
(setq ansi-term-color-vector
      [term term-color-black term-color-red term-color-green term-color-yellow 
            term-color-blue term-color-magenta term-color-cyan term-color-white])
(put 'erase-buffer 'disabled nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cperl-array-face ((t (:foreground "navy" :weight bold))))
 '(cperl-hash-face ((t (:foreground "brown" :slant italic :weight bold))))
 '(cperl-nonoverridable-face ((t (:foreground "dark green"))))
 '(custom-group-tag ((t (:inherit variable-pitch :foreground "dark slate blue" :weight bold :height 1.2))))
 '(custom-variable-tag ((t (:foreground "dark slate blue" :weight bold))))
 '(error ((t (:foreground "firebrick4" :weight bold))))
 '(font-lock-comment-face ((t (:foreground "#806080" :slant italic)))))

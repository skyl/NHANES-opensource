

% gives seqn, imq010, imq020, imq030 as columns
imq = dlmread('csv/quest/IMQ.csv', ',', 1, 1);

% seqn","ecq010","ecq020","ecq030","ecq040","ecq060",
% "ecd070a","ecd070b","ecq080","ecq090","ecd100","ecd110",
% "fsq121","ecq130","ecq140"
ecq = dlmread('csv/quest/ECQ.csv', ',', 1, 1);

% how do we join on seqn and keep labels?




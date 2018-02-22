function gwdraw()
% Draw Gridworld and robot.
  
global GWXSIZE;
global GWYSIZE;
global GWPOS;
global GWFEED;
global GWTERM;

cla;
hold on;
title('Feedback Map');
xlabel('Y');
ylabel('X');
axis equal;
axis ij;
h = imagesc(GWFEED);
hold on;

% Create a gray rectangle for the robot
rectangle('Position',[GWPOS(2)-0.5, GWPOS(1)-0.5, 1, 1], 'FaceColor', [0.5,0.5,0.5]);

% If you want to see the color scale of the world you can uncomment this
% line. This will slow down the drawing significantly.
%colorbar;
   
for x = 1:GWXSIZE
  for y = 1:GWYSIZE
    if GWTERM(x,y)
      % green circle for the goal
      radius = 0.5; 
      rectangle('Position',[y-0.5, x-0.5, radius*2, radius*2],...
      'Curvature',[1,1],...
      'FaceColor','g');
    end
  end
end

% If you want to make the robot move slower (to make it easier to
% understand what it does) you can uncomment this line.
%pause(0.1);

drawnow;
hold on;

    




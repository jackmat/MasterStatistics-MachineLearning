function gwplotallarrows(Q)
% Plot all arrows using gwplotarrow and the Q matrix.
  
global GWXSIZE;
global GWYSIZE;
global GWTERM;

% Arrow directions
% Change this to select arrow directions from the Q matrix.
A = ones(GWXSIZE, GWYSIZE);

for x = 1:GWXSIZE
    for y = 1:GWYSIZE
        if ~GWTERM(x,y)
            gwplotarrow([x y], A(y, x));
        end
    end
end

drawnow;
hold on;

    




import { of } from 'rxjs';

of(1, 2, 3).subscribe(x => console.log("The observer got a value: "+x))

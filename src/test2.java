import java.util.Arrays;
import java.util.ArrayList;

class Slice {

    int cap;
    int len;
    ArrayList<Object> wrapped;

    public Slice(Object[] vals){
        this.wrapped = new ArrayList<Object>();
        addArrayElems(vals);
        this.cap = this.wrapped.size();
        this.len = this.cap;
    }

    public Slice(){
        this.wrapped = new ArrayList<Object>();
        this.cap = 0;
        this.len = 0;
    }

    // usage go code: x = append(a, 1); --> generated: x.append(a,1);
    public void append(Slice appendTo, Object val){

        if (appendTo.mustReallocate()){
            // allocate new array if appendTo.len == appendTo.cap
            this.wrapped = new ArrayList<Object>(appendTo.wrapped);
            this.wrapped.add(val);
            this.cap = (appendTo.cap == 0) ? 1 : appendTo.cap * 2;
            System.out.println("reallocated in append()");
        }
        else{
            // copy underlying array
            this.wrapped = appendTo.wrapped;
            if (appendTo.wrapped.size() == appendTo.len){
                // must have a value here before trying to replace it with set
                appendTo.wrapped.add(appendTo.len, val);
            }
            else {
                // set replace's the value at appendTo.len
                appendTo.wrapped.set(appendTo.len, val);
            }
            this.cap = appendTo.cap;
        }
        // increment this.len only.
        this.len = appendTo.len + 1;
    }


    public void addAtIndex(int idx, Object val){
        // this function is used whenever a slice idx is assigned to.
        if (mustReallocate()){
            this.wrapped = new ArrayList<Object>(this.wrapped);
            // this.cap *= 2;
            this.cap = (this.cap == 0) ? 1 : this.cap * 2;
            System.out.println("reallocated in addAtIndex()");
        }
        this.wrapped.add(idx, val);
        this.len++;
    }

    public Object getAtIndex(int idx){
        return this.wrapped.get(idx);
    }

    public boolean mustReallocate(){
        // if len == cap we must create a new underlying array and cp all of this obj's elems
        // otherwise we can append the value to this object and return this object.
        return len == cap;
    }

    public void addArrayElems(Object[] vals){
        for (Object val : vals){
            this.wrapped.add(val);
        }
    }

    @Override
    public boolean equals(Object o) { 
        System.out.println("Attempted to use '==' on a slice. This is not valid in Go.");
        System.exit(1);
        return false;
    }

}


class test2 {
	public static void main(String[] args) {
		Slice __goLite__a2 = new Slice();
		
		{
			int __goLite__i3 = 0;
			
			while ((__goLite__i4 < 10)) {
				System.out.println(" Cap :" + " " + __goLite__a2.length + " " + ", len :" + " " + __goLite__a2.length);
				__goLite__a2.append(__goLite__a2, 0);
				try {
				} catch (Exception e) {
				} finally {
					__goLite__i5++;
				}
			}
		}
	}


	
}
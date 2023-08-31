package com.cf.parking.facade.constant;

import java.util.HashMap;
import java.util.Map;

/**
 * @author: lpy
 * @Date: 2023/03/27
 */
public class ParkingSysCodeConstant {

    /**
     * 园区和parkSysCode对应关系map
     */
    public static final Map<String, String> parkingSysCodeMap = new HashMap();

    static {
        parkingSysCodeMap.put("老园区", "6dc5132a6a7046c09ffd7be54d27ea49");
        parkingSysCodeMap.put("新园区南门", "5a277524d2bc408bbd3097e7ccaf2208");
        parkingSysCodeMap.put("地下停车场", "aaed725983664c7aa0a1a4dddba3f05c");
        parkingSysCodeMap.put("组装车间1F", "2457999fe2914251976fd333d2816fb2");
        parkingSysCodeMap.put("组装车间4F停车库", "4680b7e1ec414a5ebdf48127f73acd71");
//        parkingSysCodeMap.put("新厂区物流门", "4a6c46727d66489585736a69f214cace");
    }

}

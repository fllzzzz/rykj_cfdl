package com.cf.parking.services.utils;

import com.cf.parking.facade.enums.BizResultCodeEnum;
import com.cf.support.exception.BusinessException;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.Collection;

/**
 * @author: lpy
 * @Date: 2022/11/10
 */


@Slf4j
public class LongLatitudeUtils {
    private static Double EARTH_RADIUS = 6378.137;

    private static Double rad(Double d) {
        return d * Math.PI / 180.0;
    }

    /**
     * 根据经纬度获取距离
     *
     * @param startLong            起始地经度
     * @param startLati            起始地纬度
     * @param destinationLongitude 到达地经度
     * @param destinationLatitude  到达地纬度
     * @return km距离
     */
    public static Double getDistance(BigDecimal startLong, BigDecimal startLati, BigDecimal destinationLongitude, BigDecimal destinationLatitude) {
        LongLatitudeUtils.checkLonLatLegal(Arrays.asList(startLong, startLati, destinationLongitude, destinationLatitude));
        Double startLon = startLong.doubleValue();
        Double startLat = startLati.doubleValue();
        Double destinationLon = destinationLongitude.doubleValue();
        Double destinationLat = destinationLatitude.doubleValue();

        Double startLatitude = rad(startLat);
        Double radLat2 = rad(destinationLat);
        Double a = startLatitude - radLat2;
        Double b = rad(startLon) - rad(destinationLon);
        Double s = 2 * Math.asin(Math.sqrt(Math.pow(Math.sin(a / 2), 2)
                + Math.cos(startLatitude) * Math.cos(radLat2)
                * Math.pow(Math.sin(b / 2), 2)));
        s = s * EARTH_RADIUS;
        // 保留两位小数
        return Math.round(s * 100d) / 100d;
    }

    /**
     * 校验地理位置是否合法
     *
     * @param list 经纬度列表
     */
    public static void checkLonLatLegal(Collection<BigDecimal> list) {
        list.stream().forEach(o -> {
            if (ObjectUtils.isEmpty(o) || BigDecimal.ZERO.equals(o)) {
                throw new BusinessException(BizResultCodeEnum.FAILED_TO_GET_GEOGRAPHIC_LOCATION.getMsg());
            }
        });
    }

    // test
    public static void main(String[] args) {
        checkLonLatLegal(Arrays.asList(BigDecimal.valueOf(0), BigDecimal.valueOf(120.124597)));
    }
}

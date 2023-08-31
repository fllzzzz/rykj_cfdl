package com.cf.parking.api.controller;

import com.cf.parking.api.annotation.AdminOptLogTitle;
import com.cf.parking.api.response.UploadUrlRsp;
import com.cf.parking.facade.enums.BizResultCodeEnum;
import com.cf.parking.facade.enums.UploadFileTypeEnum;
import com.cf.parking.services.properties.AliyunAccessProperties;
import com.cf.support.result.Result;
import com.cf.support.utils.OssUploadUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import javax.annotation.Resource;
import java.io.InputStream;
import java.util.UUID;

/**
 * @author weihui
 * @date 2020/10/22
 */
@Slf4j
@RestController
@RequestMapping("/upload")
public class UploadController {

    @Resource
    private AliyunAccessProperties accessConfig;

    @AdminOptLogTitle("文件上传")
    @PostMapping("/uploadFile")
    public Result<UploadUrlRsp> uploadFile(@RequestParam("file") MultipartFile file, @RequestParam("type") Integer type){
        if(null == file || null == type){
            return Result.buildResult(BizResultCodeEnum.PARAM_NULL);
        }

        //存储路径
        String filePrefix = UploadFileTypeEnum.getPath(type);
        if(StringUtils.isBlank(filePrefix)){
            return Result.buildResult(BizResultCodeEnum.PARAM_ERROR);
        }

        //获取上传文件名,包含后缀
        String originalFilename = file.getOriginalFilename();
        //获取后缀
        String substring = originalFilename.substring(originalFilename.lastIndexOf("."));
        //保存的文件名
        String uuid = UUID.randomUUID().toString().replaceAll("-", "").toUpperCase();

        String fileName = filePrefix + uuid + substring;
        //文件上传
        try{
            String path = uploadFile2Oss(fileName,file.getInputStream());
            if(StringUtils.isBlank(path)){
                return Result.buildErrorResult("上传失败，请稍后再试");
            }
            UploadUrlRsp data = new UploadUrlRsp();
            data.setOssUrl(accessConfig.getOssUrl());
            data.setPath(path);
            return Result.buildSuccessResult(data);
        }catch (Exception e){
            log.error("文件上传异常,e",e);
            return Result.buildErrorResult("上传失败，请稍后再试");
        }
    }

    private String uploadFile2Oss(String fileName, InputStream inputStream) {
        try {
            long tm = System.currentTimeMillis();
            log.info("开始上传文件到oss");
            String path = OssUploadUtils.getInstance(accessConfig.getOssEndPoint(), accessConfig.getKey()
                    , accessConfig.getSecret(), accessConfig.getBucketName())
                    .uploadByFile(inputStream, fileName);
            log.info("上传文件到oss耗时,rt={}", (System.currentTimeMillis() - tm));
            if (StringUtils.isNotBlank(path)) {
                return path;
            }
        } catch (Exception e) {
            log.error("uploadFile2Oss,e=", e);
        }
        return null;
    }
}
